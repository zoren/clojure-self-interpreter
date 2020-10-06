;; https://clojure.org/reference/evaluation

;; Strings, numbers, characters, true, false, nil and keywords evaluate to themselves.
(defn constant? [value]
  (some #(% value)
        [string?
         number?
         char?
         (partial contains? #{true false nil})
         keyword?]))

(defn meta-var? [exp] (and (symbol? exp) (= "meta" (namespace exp))))

(defn meta-var [] (symbol "meta" (name (gensym "v"))))

(defn compare-any [x y]
  (let [[tx ty] (map type [x y])]
    (if (= tx ty)
      (compare x y)
      ;; todo does not work for closures among many other values
      (compare (.getName tx) (.getName ty)))))

(defn normalize-constraint [constraint]
  (when-not (seq? constraint) (throw (ex-info "not a seq" {:constraint constraint :type (type constraint)})))
  (when-not (first constraint) (throw (ex-info "no first" {})))
  (case (-> constraint first resolve symbol str)
    "clojure.core/="
    (do
      (when-not (= (count constraint) 3) (throw (ex-info "only two args supported" {})))
      (when (every? (comp not meta-var?) (rest constraint)) (throw (ex-info "constraint must have at least one meta-variable" {})))
      (conj (sort-by identity compare-any (conj (rest constraint) 'clojure.core/=))))
    "clojure.core/and"
    (do
      (when-not (= (count constraint) 3) (throw (ex-info "only two args supported" {})))
      (when (every? (comp not meta-var?) (rest constraint)) (throw (ex-info "constraint must have at least one meta-variable" {})))
      (conj (sort-by identity compare-any (conj (rest constraint) 'clojure.core/=))))
    (throw (ex-info "unknown operator" {:operator (first constraint)}))))

(comment
  (normalize-constraint '3)
  (normalize-constraint '())
  (normalize-constraint '(= 3 4 5))
  (normalize-constraint '(= 3 4))
  (normalize-constraint '(= x 5))
  (def mvar (meta-var))
  (normalize-constraint `(= ~mvar 5))
  (normalize-constraint `(= ~(meta-var) 5))
  (normalize-constraint '(= meta/x 5))
  (normalize-constraint '(= 5 meta/x))
  )

(defn add-constraint [constraint-set constraint]
  (conj constraint-set (normalize-constraint constraint)))

(defn double-negation-elim [exp]
  (case (-> exp first resolve symbol)
    'clojure.core/not
    (case (-> (second exp) first resolve symbol)
      'clojure.core/not
      (double-negation-elim (second (second exp)))
      `(not ~(double-negation-elim exp)))
    exp
    )
  )

;; we define a meta evaluator meval
;; given an expression that can contain meta-variables
;; and a set of constraints on the metavariables
;; returns a new set of constraints and a metavariable for it's result
(defn meval [constraints exp]
  (let
      [meval* (fn [constraints exps]
                (reduce
                 (fn [[vars cs] exp]
                   (let [[mv cs2] (meval cs exp)] [(conj vars mv) cs2]))
                 [[] constraints]
                 exps))]
    (cond
      (constant? exp)
      (let [mres (meta-var)]
        [mres (add-constraint constraints `(= ~mres ~exp))])
      (meta-var? exp)
      (let [mres (meta-var)]
        [mres (add-constraint constraints `(= ~mres ~exp))])
      (seq? exp)
      (do
        (when-not (first exp) (throw (ex-info "no first" {})))
        (case (-> exp first resolve symbol str)
          "clojure.core/="
          (let [mres (meta-var)
                [[x y] constraints2] (meval* constraints (rest exp))]
            (when-not (= (count exp) 3) (throw (ex-info "only two args supported" {})))
                                        ;(conj (sort-by identity compare-any (conj (rest constraint) 'clojure.core/=)))
            [mres (add-constraint constraints2 `(or (and (= ~x ~y) (= ~mres true)) (and (not (= ~x ~y)) (= ~mres false))))])
          "clojure.core/and"
          (let [mres (meta-var)
                [[x y] constraints2] (meval* constraints (rest exp))]
            (when-not (= (count exp) 3) (throw (ex-info "only two args supported" {})))
            [mres (add-constraint constraints2 `(and ~x ~y))])
          (throw (ex-info "meval: unknown function" {:operator (first exp)}))))
      :else
      (throw (ex-info "meval: expression not supported" {:exp exp})))))

(comment
  (def if-test-cases '[(if false 1 2) (if nil 1 2) (if 0 1 2) (if m/c 1 2)])
  (def and-test-cases [(and false nil) (and nil false) (and 1 false) (and 1 nil) (and 1 2) (and nil 1)])

  (meval #{} '(if meta/cond 1 2))
  '[m/res #{(and meta/cond (= m/res 1))
            (and (not meta/cond) (= m/res 2))}]

  (meval '#{(= meta/cond nil) (= meta/cond 5)} '(if meta/cond 1 2))
  '[m/res #{(and (= meta/cond nil) (= m/res 1))
            (and (= meta/cond 5) (= m/res 2))}]

  (meval #{} '(and true false))
  ;; [m/result '#{(= m/result 4)}]

  (meval (add-constraint #{} '(= meta/var 5)) 'meta/var)
  ;; [m/result '#{(= m/var 5) (= m/result 4)}]

  (meval #{} '(= 5 5))
  ;; [m/result '#{(= m/result true)}]
  (meval '(= 5 6) '#{})
  ;; [m/result '#{(= m/result false)}]

  (meval '(= m/s 5) '#{})
  [m/result #{(and (= m/s 5) (= m/result false))
              (and (not (= m/s 5)) (= m/result true))
              }]
  (meval '(= m/s 5) '#{(string? m/s)})
  ;; [m/result #{(= m/result false)}]


  )
