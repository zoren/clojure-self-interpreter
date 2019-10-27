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
    (cond
      (constant? form)
      form

      (symbol? form)
      (let [value (get context form :not-found)]
        (if (= :not-found value)
          (throw-context (str "Unable to resolve symbol: " (name form) " in this context")))
        (:constant value))

      (list? form)
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
                             (if-not (symbol? (first pair))
                               (throw-context (str "Bad binding form, expected symbol, got: " (first pair))))
                             (assoc acc (first pair) {:constant (meval acc (second pair))})) context bindings)]
              (last (map (partial meval context') (rest form)))))
          "fn*"
          (let
              [arity-folder
               (fn [acc body]
                 (if-not (or (empty? body) (list? body)) (throw-context nil))
                 (let [params (first body)]
                   (if-not (vector? params) (throw-context (str (.getName (type (nth form 1 nil))) " cannot be cast to clojure.lang.ISeq")))
                   (if-not (every? symbol? params) (throw-context "fn params must be Symbols"))
                   (if (acc (count params)) (throw-context "Can't have 2 overloads with same arity"))
                   (assoc acc (count params) body)))]
            {:fn (try (arity-folder {} (rest form))
                      (catch Throwable _t
                        (reduce arity-folder {} (rest form))))})
          (throw-context "case not supported"))))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (constant? evalue)
      {:constant evalue}
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn run-compare [form]
  (let [check
        #(try {:value (% form)} (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (check (partial meval {}))]
    (if (not= reference this)
      (throw (ex-info "Difference between reference and this implementation" {:form form :reference reference :this this})))))

(def tests
  '["" 5 5.0 5.00M \a \" true false nil :kw
    not-found ()
    (if) (if nil) (if 0 1 2 3) (if false 1 2) (if nil 1 2) (if true 1 2) (if true 1) (if false 1)
    (do) (do 4) (do 4 5)
    (let*) (let* [x]) (let* [4 4] 5) (let* []) (let* [x 5] 6) (let* [x 5] 1 2 3)  (let* [x 5 y x] y)
    ])
(doseq [f tests] (run-compare f))

(defn run-compare-no-cause-compare [form]
  (let [check
        #(try {:value (% form)} (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (check (partial meval {}))
        throw-compare (fn [message] (throw (ex-info message {:form form :reference reference :this this})))]
    (cond
      (and (contains? reference :value) (contains? this :value))
      (if (fn? (reference :value))
        (if-not (contains? (:value this) :fn)
          (throw-compare "Difference between reference and this implementation: function"))
        (if (not= reference this)
          (throw-compare "Difference between reference and this implementation")))

      (and (contains? reference :cause) (contains? this :cause))
      nil
      :else
      (throw-compare "Difference between reference and this implementation"))))

;; eval gives poor exception cause messages for fn* syntax errors so let's not copy those
(def fn*-tests
  '[(fn* {}) (fn* 3) (fn* "") (fn* [3]) (fn* ())
    (fn*) (fn* [x]) (fn* [x] x) (fn* [x] 5 x)
    (fn* ({})) (fn* ([]) ([])) (fn* ([x]) ([x]))
    (fn* ([x] 1) ([x y] 2))
    ])
(doseq [f fn*-tests] (run-compare-no-cause-compare f))
