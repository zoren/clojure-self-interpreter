(defn meval
  [context form]
  (if (symbol? form)
    (let [value (get context form :not-found)]
      (if (= :not-found value)
        (throw (ex-info (str "Unable to resolve symbol: " (name form) " in this context") {:context context :form form})))
      (first value))
    (if (number? form)
      {:num form}
      (throw (ex-info "case not supported" {:context context :form form})))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (number? evalue)
      {:num evalue}
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn test-eval
  ([form] (test-eval {} form))
  ([context form] (meval (into {} (map (fn [[k v]] [k (lift-value v)]) context)) form)))

(comment
  (test-eval '5)
  (test-eval 'not-found)
  (test-eval '{x 45} 'x)
  )
