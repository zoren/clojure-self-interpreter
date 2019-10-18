(defn meval
  [context form]
  (if (symbol? form)
    (let [value (get context form :not-found)]
      (if (= :not-found value)
        (throw (ex-info (str "Unable to resolve symbol: " (name form) " in this context") {:context context :form form})))
      (first value))
    (if (number? form)
      form
      (throw (ex-info "case not supported" {:context context :form form}))
      )))

(defn test-eval
  ([form] (test-eval {} form))
  ([context form] (meval (into {} (map (fn [[k v]] [k [v]]) context)) form)))

(comment
  (test-eval '5)
  (test-eval 'x)
  (test-eval '{x 45} 'x)
  (test-eval '(let [x 5] x))
  )
