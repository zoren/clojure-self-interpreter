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
  [form]
  (letfn [(throw-context [context message]
            (throw (ex-info message {:context context :form form})))
          (throw-syntax [message]
            (throw (ex-info message {:form form})))]
    (cond
      (constant? form)
      (fn [_context] form)

      (symbol? form)
      (fn [context]
        (let [value (get context form :not-found)]
          (if (= :not-found value)
            (throw-context context (str "Unable to resolve symbol: " (name form) " in this context")))
          (:constant value)))

      (vector? form)
      (let [eelem (mapv meval form)]
        (fn [context] (mapv #(% context) eelem)))

      (set? form)
      (let [eelem (mapv meval form)]
        (fn [context]
          (reduce
           (fn [acc ce]
             (let [elem (ce context)]
               (if (contains? acc elem) (throw-context context (str "Duplicate key: " elem)))
               (conj acc elem)))
           #{}
           eelem)))

      (map? form)
      (let [eelem (mapv (fn [[k v]] (mapv meval [k v])) form)]
        (fn [context]
          (reduce
           (fn [acc kv]
             (let [[k v] (mapv #(% context) kv)]
               (if (contains? acc k) (throw-context context (str "Duplicate key: " k)))
               (assoc acc k v)))
           {}
           eelem)))

      (list? form)
      (if (empty? form)
        (fn [_context] '())
        (case (name (first form))
          "if"
          (case (count form)
            3
            (let [[ec et] (map meval (rest form))]
              #(if (ec %) (et %)))
            4
            (let [[ec et ef] (map meval (rest form))]
              #(if (ec %) (et %) (ef %)))
            (if (< (count form) 3)
              (throw-syntax "Too few arguments to if")
              (throw-syntax "Too many arguments to if")))

          "do"
          (let [ces (map meval (rest form))]
            (fn [context] (last (map #(% context) ces))))

          "let*"
          (do
            (if-not (vector? (nth form 1 nil))
              (throw-syntax "Bad binding form, expected vector"))
            (if (odd? (count (nth form 1)))
              (throw-syntax "Bad binding form, expected matched symbol expression pairs"))
            (let [cbindings
                  (mapv (fn [[bname bexp]]
                          (if-not (symbol? bname)
                            (throw-syntax (str "Bad binding form, expected symbol, got: " bname)))
                          [bname (meval bexp)]
                          ) (partition 2 (nth form 1)))
                  cbody (meval (last (drop 2 form)))]
              (fn [context]
                (cbody (reduce (fn [acc [k v]] (assoc acc k {:constant (v acc)}))
                               context
                               cbindings)))))

          "fn*"
          (let
              [arity-folder
               (fn [acc body]
                 ;; (if-not (or (empty? body) (list? body)) (throw-syntax ""))
                 (let [params (first body)]
                   (if-not (vector? params) (throw-syntax (str body params " cannot be cast to clojure.lang.ISeq")))
                   (if-not (every? symbol? params) (throw-syntax "fn params must be Symbols"))
                   (if (acc (count params)) (throw-syntax "Can't have 2 overloads with same arity"))
                   (assoc acc (count params) (mapv meval (rest body)))))
               m (try
                   (arity-folder {} (rest form))
                   (catch Throwable _t
                     (reduce arity-folder {} (rest form))))]
            (fn [context]
              {:fn
               ;;(try
               m
               ;; (catch Throwable _t
               ;;   (reduce arity-folder {} (rest form)))
               ;; )
               }))

          (throw-syntax "list case not supported")))

      :else
      (throw-syntax "case not supported"))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (constant? evalue)
      {:constant evalue}
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn run-compare [form]
  (let [check
        #(try {:value (% form)} (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (check (fn [form] ((meval form) {})))]
    (if (not= reference this)
      (throw (ex-info "Difference between reference and this implementation"
                      {:form form :reference reference :this this})))))

(def tests
  '["" 5 5.0 5.00M \a \" true false nil :kw
    not-found
    [] [(if true 1)] [1 3 4]
    #{} #{1 (if true 1)} #{1 2 3}
    {} {1 (if true 1) (if true 1) 1} {:k 2 :v 5}
    ()
    (if) (if nil) (if 0 1 2 3) (if false (if)) (if false 1 2) (if nil 1 2) (if true 1 2) (if true 1) (if false 1)
    (do) (do 4) (do 4 5) (do (if) 5)
    (let*) (let* [x]) (let* [4 4] 5) (let* []) (let* [x 5] 6) (let* [x 5] x) (let* [x 5] 1 2 3)  (let* [x 5 y x] y)
    ])
(doseq [f tests] (run-compare f))

(defn run-compare-no-cause-compare [form]
  (let [check
        #(try {:value (% form)} (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (check (fn [form] ((meval form) {})))
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
    (fn* [x]) (fn* [x] x) (fn* [x] 5 x)
    (fn*) (fn* ({})) (fn* ([]) ([])) (fn* ([x]) ([x]))
    (fn* ([x] 1) ([x y] 2))
    ;; when bodies of functions have syntax errors they should be reported immediately not at call time
    (fn* [] (if))
    ])
(doseq [f fn*-tests] (run-compare-no-cause-compare f))

(comment
  (def js-spec  '#{if def fn* do let* loop* letfn* throw try catch finally
                   recur new set! ns deftype* defrecord* . js* & quote var})
  (def java-spec '#{& monitor-exit case* try reify* finally loop* do letfn* if
                    clojure.core/import* new deftype* let* fn* recur set! . var quote
                    catch throw monitor-enter def})
  (clojure.set/intersection js-spec java-spec)
  )
