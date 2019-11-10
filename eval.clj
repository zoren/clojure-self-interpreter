;; https://clojure.org/reference/evaluation

;; Strings, numbers, characters, true, false, nil and keywords evaluate to themselves.
(defn constant? [value]
  (some #(% value)
        [string?
         number?
         char?
         (partial contains? #{true false nil})
         keyword?]))

(defn wrap [value] {:value value})
(defn wrapped? [m] (contains? m :value))
(defn exception? [m] (contains? m :exception))
(defn unwrap [m]
  (if (wrapped? m)
    (:value m)
    (if (contains? m :fn)
      m
      (throw (ex-info "could not unwrap" {:value m})))))
(defn exception [value] {:exception value})
(defn wrap-if-not [value] (if (exception? value) value (wrap value)))

(defn meval
  [form]
  (letfn [(throw-context [context message]
            (exception {:message message :context context :form form}))
          (throw-syntax [message]
            (throw (ex-info message {:form form})))
          (map-meval [context cexps]
            (reduce (fn [acc fin]
                      (let [v (fin context)]
                        (if (exception? v)
                          (reduced v)
                          (conj acc (unwrap v)))))
                    []
                    cexps))
          (apply-func [form]
            (let [ces (mapv meval form)]
              (fn [context]
                (let [evs (map-meval context ces)]
                  (if (exception? evs)
                    evs
                    (if (not (contains? (first evs) :fn)) (throw-context context (str (first evs) " cannot be cast to clojure.lang.IFn"))
                        (let [args (rest evs)]
                          (if-let [fn-body ((:fn (first evs)) (count args))]
                            (let [context' (reduce (fn [acc [param arg]] (assoc acc param (wrap arg)))
                                                   (:context (first evs)) (map vector (:params fn-body) args))]
                              ((:f fn-body) context'))
                            (throw-context context (str "Wrong number of args (" (count args) ") passed to function"))))))))))]
    (cond
      (constant? form)
      (fn [_context] (wrap form))

      (symbol? form)
      (fn [context]
        (let [value (get context form :not-found)]
          (if (= :not-found value)
            (throw-context context (str "Unable to resolve symbol: " (name form) " in this context"))
            value)))

      (vector? form)
      (let [eelem (mapv meval form)]
        (fn [context] (wrap-if-not (map-meval context eelem))))

      (set? form)
      (let [eelem (mapv meval form)]
        (fn [context]
          (let [ev-forms (map-meval context eelem)]
            (if (exception? ev-forms)
              ev-forms
              (wrap-if-not
               (reduce (fn [acc v]
                         (if (contains? acc v)
                           (reduced (throw-context context (str "Duplicate key: " v)))
                           (conj acc v)))
                       #{}
                       ev-forms))))))

      (map? form)
      (let [eelem (mapv (fn [[k v]] (mapv meval [k v])) form)]
        (fn [context]
          (wrap-if-not
           (reduce
            (fn [acc kv]
              (let [ekv (map-meval context kv)]
                (if (exception? ekv)
                  ekv
                  (let [[k v] ekv]
                    (if (contains? acc k)
                      (reduced (throw-context context (str "Duplicate key: " k)))
                      (assoc acc k v))))))
            {}
            eelem))))

      (list? form)
      (if (empty? form)
        (fn [_context] (wrap ()))
        (if (symbol? (first form))
          (case (name (first form))
            "if"
            (case (count form)
              3
              (let [[ec et] (map meval (rest form))]
                #(if (unwrap (ec %)) (et %) (wrap nil)))
              4
              (let [[ec et ef] (map meval (rest form))]
                #(if (unwrap (ec %)) (et %) (ef %)))
              (if (< (count form) 3)
                (throw-syntax "Too few arguments to if")
                (throw-syntax "Too many arguments to if")))

            "do"
            (let [ces (mapv meval (rest form))]
              (fn [context] (wrap (last (map #(unwrap (% context)) ces)))))

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
                  (cbody (reduce (fn [acc [k v]] (assoc acc k (v acc)))
                                 context
                                 cbindings)))))

            "fn*"
            (let
                [arity-folder
                 (fn [acc body]
                   (let [params (first body)]
                     (if-not (vector? params) (throw-syntax (str body params " cannot be cast to clojure.lang.ISeq")))
                     (if-not (every? symbol? params) (throw-syntax "fn params must be Symbols"))
                     (if (acc (count params)) (throw-syntax "Can't have 2 overloads with same arity"))
                     (let [cbody (mapv meval (rest body))]
                       (assoc acc (count params)
                              {:params params
                               :f (fn [context] (let [evs (map-meval context cbody)]
                                                  (if (exception? evs) evs (wrap (last evs)))))}
                              ))))
                 m (try
                     (arity-folder {} (rest form))
                     (catch Throwable _t
                       (reduce arity-folder {} (rest form))))]
              (fn [context] {:fn m :context context}))

            (do
              (if (special-symbol? (first form))
                (throw-syntax "special case not supported"))
              (apply-func form)))
          ;; list does not start with symbol
          (apply-func form)
          ))

      :else
      (throw-syntax "case not supported"))))

(defn lift-value [value]
  (let [evalue (eval value)]
    (if (constant? evalue)
      (wrap evalue)
      (throw (ex-info "value could not be lifted" {:value evalue})))))

(defn run-this [form]
  (let [eval-form
        (try (meval form)
             (catch Throwable e {:cause (:cause (Throwable->map e))}))]
    (if (fn? eval-form)
      (let [v (eval-form {})]
        (if-not (map? v) (throw (ex-info "cannot" {:form form :v v})))
        (cond
          (contains? v :value) v
          (contains? v :fn) v
          (contains? v :exception) {:cause (:message (:exception v))}
          :else (throw (ex-info "cannot" {:form form :v v}))
          ))
      eval-form)))

(defn run-compare [form]
  (let [check
        #(try {:value (% form)}
              (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (run-this form)]
    (if (not= reference this)
      (throw (ex-info "Difference between reference and this implementation"
                      {:form form :reference reference :this this})))))

(def tests
  '["" 5 5.0 5.00M \a \" true false nil :kw
    not-found
    [] [(if true 1)] [1 3 4] [not-found] [1 (if true 1)]
    #{} #{1 (if true 1)} #{1 2 3} #{1 (if true 1) (if)}
    {} {1 (if true 1) (if true 1) 1} {:k 2 :v 5} {not-found 3} {5 not-found}
    ()
    (if) (if nil) (if 0 1 2 3) (if false (if)) (if false 1 2) (if nil 1 2) (if true 1 2) (if true 1) (if false 1)
    (do) (do 4) (do 4 5) (do (if) 5)
    (let*) (let* [x]) (let* [4 4] 5) (let* []) (let* [x 5] 6) (let* [x 5] x) (let* [x 5] 1 2 3)  (let* [x 5 y x] y)
    ((fn* [x] x) 5)
    (let* [f (fn* [x] x)] (f 5))
    ])
(doseq [f tests] (run-compare f))

(defn run-compare-no-cause-compare [form]
  (let [check
        #(try {:value (% form)}
              (catch Throwable e {:cause (:cause (Throwable->map e))}))
        reference (check eval)
        this (run-this form)
        throw-compare (fn [message] (throw (ex-info message {:form form :reference reference :this this})))]
    (cond
      (and (contains? reference :value)
           (or (contains? this :value) (contains? this :fn)))
      (if (fn? (reference :value))
        (if-not (contains? this :fn)
          (throw-compare "Difference between reference and this implementation: function"))
        (if (not= reference this)
          (throw-compare "Difference between reference and this implementation")))

      (and (contains? reference :cause) (contains? this :cause))
      (if (clojure.string/includes? (this :cause) "not supported")
        (throw-compare "should not be because it's not supported")
        )
      :else
      (throw-compare "Difference between reference and this implementation"))))

;; eval gives poor exception cause messages for fn* syntax errors so let's not copy those
(def fn*-tests
  '[(fn* {}) (fn* 3) (fn* "") (fn* [3]) (fn* ())
    (fn* [x]) (fn* [x] x) (fn* [x] 5 x)
    (fn*) (fn* ({})) (fn* ([]) ([])) (fn* ([x]) ([x]))
    (fn* ([x] 1) ([x y] 2))
    (fn* ([x] 1) ([x & y] 2))
    ;; when bodies of functions have syntax errors they should be reported immediately not at call time
    (fn* [] (if))
    (((fn* [x] (fn* [y] x)) "returned") "ignored")
    (fn* [x] x)
    (let* [f (fn* [x] x)] f)
    ((fn* [x] x) (/ 1 0))
                                        ;    ((fn* [& c]))  ((fn* [& c]) 3)  ((fn* [& c]) 3 3)
    ((fn* [] (/ 1 0) 0))
    ])
(doseq [f fn*-tests] (run-compare-no-cause-compare f))

(comment
  ;; from cljs (source special-symbol?)
  (def js-spec
    '#{if def fn* do let* loop* letfn* throw try catch finally
       recur new set! ns deftype* defrecord* . js* & quote var})
  ;;  https://stackoverflow.com/a/30947787
  (def java-spec (into #{} (keys (. clojure.lang.Compiler specials))))
  (clojure.set/intersection js-spec java-spec)
  )
