;; Strings, numbers, characters, true, false, nil and keywords evaluate to themselves.
;; https://clojure.org/reference/evaluation
;;An empty list () evaluates to an empty list.

;; Strings, numbers, characters, true, false, nil and keywords evaluate to themselves.
(defn constant? [value]
  (some #(% value)
        [string?
         number?
         char?
         (partial contains? #{true false nil})
         keyword?]))

(defn meval
  [context form]
  (if (symbol? form)
    (let [value (get context form :not-found)]
      (if (= :not-found value)
        (throw (ex-info (str "Unable to resolve symbol: " (name form) " in this context") {:context context :form form})))
      (first value))
    (if (constant? form)
      {:constant form}
      (throw (ex-info "case not supported" {:context context :form form})))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (constant? evalue)
      {:constant evalue}
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn test-eval
  ([form] (test-eval {} form))
  ([context form] (meval (into {} (map (fn [[k v]] [k (lift-value v)]) context)) form)))

(comment
  (test-eval '5)
  (test-eval '"")
  (test-eval ':h)
  (test-eval 'not-found)
  (test-eval '{x 45} 'x)
  )
