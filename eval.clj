;; https://clojure.org/reference/evaluation

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
            (case (name (first form))
              "if"
              (cond
                (= (count form) 3) (if (meval' (nth form 1))
                                     (meval' (nth form 2)))
                (= (count form) 4) (if (meval' (nth form 1))
                                     (meval' (nth form 2))
                                     (meval' (nth form 3)))
                (< (count form) 3) (throw-context "Too few arguments to if")
                :else (throw-context "Too many arguments to if"))
              "do"
              (last (map meval' (rest form)))
              "let*"
              (do
                (if-not (vector? (nth form 1 nil))
                  (throw-context "Bad binding form, expected vector"))
                (if (odd? (count (nth form 1)))
                  (throw-context "Bad binding form, expected matched symbol expression pairs"))
                (let* [bindings (partition 2 (nth form 1))
                       context'
                       (reduce (fn [acc pair]
                                 (if-not (symbol? (first pair)) (throw-context (str "Bad binding form, expected symbol, got: " (first pair))))
                                 (assoc acc (first pair) {:constant (meval acc (second pair))})) context bindings)]
                  (last (map (partial meval context') (rest form)))))
              (throw-context "case not supported"))))))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (constant? evalue)
      {:constant evalue}
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn run-compare [form]
  (let [check
        #(try {:value (% form)} (catch Exception e (select-keys (Throwable->map e) [:cause])))
        reference (check eval)
        this (check (partial meval {}))]
    (if (not= reference this)
      (throw (ex-info "Difference between reference and this implementation"
                      {:form form :reference reference :this this})))))

(def tests
  '[5 "" :kw () not-found true false nil
    (if) (if nil) (if 0 1 2 3) (if false 1 2) (if nil 1 2) (if true 1 2) (if true 1) (if false 1)
    (do) (do 4) (do 4 5)
    (let*) (let* [x]) (let* [4 4] 5) (let* []) (let* [x 5] 6) (let* [x 5] 1 2 3)  (let* [x 5 y x] y)
    ])
(doseq [f tests] (run-compare f))
