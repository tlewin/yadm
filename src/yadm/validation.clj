(ns yadm.validation
  (:require [clojure.string :as string]))

(defmulti validate-value (fn [params value] (:validator params)))

(defn format-validator-message
  [vparams msg]
  (cond
    ;; String
    (string? msg) (string/replace
                   msg
                   #"\{(\S+)\}"
                   (fn [[token field]]
                     (let [key (keyword field)]
                       (if-let [v (get vparams key)]
                         (cond
                           (keyword? v) (name v)
                           (seq? v) (string/join ", " v)
                           :else (str v))
                         token))))
    ;; Function
    (fn? msg) (msg vparams)
    ;; Else
    :else msg))

(defmacro defvalidator
  [vname & {:keys [body msg skip-nil?]
            :or {msg (str vname " has failed for {field}: {value}")
                 skip-nil? true}}]
  (assert (some? body) "body must be provided")
  `(defmethod validate-value ~(keyword vname) [params# value#]
     (if (or (and ~skip-nil? (nil? value#))
             (~body params# value#))
       [:ok]
       [:fail (format-validator-message
               (merge {:value value#} params#)
               (or (:msg params#) ~msg))])))

(defvalidator required
  :msg "{field} is required"
  :skip-nil? false
  :body
  (fn [params value]
    (and (some? value)
         (not (and (string? value)
                   (string/blank? value))))))

(defvalidator in
  :msg "{field} should be in {set}"
  :body
  (fn [params value]
    (let [vset (set (:set params))]
      (vset value))))

(defvalidator format
  :msg "Invalid format {value} for {field}"
  :body
  (fn [params value]
    (let [pattern (:with params)]
      (some? (re-matches pattern value)))))

(defvalidator range
  :msg "{value} not in range [{min}, {max}]"
  :body
  (fn [params value]
    (let [min (:min params)
          max (:max params)]
      (and (or (nil? min) (>= value min))
           (or (nil? max) (<= value max))))))

(defvalidator custom
  :msg "{value} fail for field {field} using custom validator"
  :body
  (fn [params value]
    (let [fn (:fn params)]
      (fn params value))))

(defn- apply-validation
  [field value [v & vargs] v-options]
  (validate-value (merge {:validator v
                          :field field}
                         (apply hash-map vargs)
                         v-options)
                  value))

(defn- apply-validation-field
  [field value validations v-options]
  (->> validations
       (reduce
        (fn [m validation]
          (conj m [(first validation) ;; Validator
                   (apply-validation field
                                     value
                                     validation
                                     v-options)]))
        [])
       (filter
        (fn [[_ [status msg]]]
          (not= status :ok)))
       (map
        (fn [[v [status msg]]]
          [v msg]))))

(defn validate
  [validation-map data & {:keys [defined-fields?]
                          :or {defined-fields? false}
                          :as v-options}]
  (->> validation-map
       (seq)
       ;; Remove unnecessary fields
       (filter
        (fn [[field _]]
          (or (not defined-fields?)
              (contains? data field))))
       ;; Apply validation for each field
       (reduce
        (fn [memo [field validations]]
          (conj memo [field
                      (apply-validation-field field
                                              (get data field)
                                              validations
                                              v-options)]))
        [])
       ;; Remove fields with no error
       (filter
        (fn [[_ errors]]
          (not-empty errors)))
       ;; Build a hashmap
       (reduce
        (fn [m [field msg]]
          (assoc m field msg))
        {})))
