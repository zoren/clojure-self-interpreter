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
  (letfn [(throw-context [message]
            (throw (ex-info message {:context context :form form})))
          (meval' [form] (meval context form))]
    (if (symbol? form)
      (let [value (get context form :not-found)]
        (if (= :not-found value)
          (throw-context (str "Unable to resolve symbol: " (name form) " in this context")))
        (:constant value))
      (if (constant? form)
        form
        (if (list? form)
          (if (empty? form)
            '()
            (case (first form)
              'if
              (cond
                (= (count form) 3) (if (meval' (nth form 1))
                                     (meval' (nth form 2)))
                (= (count form) 4) (if (meval' (nth form 1))
                                     (meval' (nth form 2))
                                     (meval' (nth form 3)))
                (< (count form) 3) (throw-context "Too few arguments to if")
                (> (count form) 4) (throw-context "Too many arguments to if")
                :else (throw-context "should not happen"))
              (throw-context "case not supported"))))))))

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
  (test-eval '())
  (test-eval 'not-found)
  (test-eval '{x 45} 'x)
  (test-eval '(if))
  (test-eval '(if nil))
  (test-eval '(if false 1 2 3))
  (test-eval '(if false 1 2))
  (test-eval '(if nil 1 2))
  (test-eval '(if true 1 2))
  (test-eval '(if true 1))
  (test-eval '(if false 1))
  )
