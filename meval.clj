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
      ;; todo does not work for closures
      (compare (.getName tx) (.getName ty)))))

(defn normalize-constraint [constraint]
  (when-not (seq? constraint) (throw (ex-info "not a seq" {:constraint constraint :type (type constraint)})))
  (when-not (first constraint) (throw (ex-info "no first" {})))
  (case (-> constraint first resolve symbol)
    'clojure.core/=
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

(defn meval [exp constraints]
  (cond
    (constant? exp)
    (let [mres (meta-var)]
      [mres (conj constraints `(= ~mres ~exp))])
    (meta-var? exp)
    4
    :else
    (throw (ex-info "meval: expression not supported" {:exp exp}))))

(comment
  ;; we define a meta evaluator meval
  ;; given an expression that can contain meta-variables
  ;; and a set of constraints on the metavariables
  ;; returns a new set of constraints and a metavariable for it's result

  (meval '4 #{})
  ;; [m/result '#{(= m/result 4)}]

  (meval 'meta/var '#{(= meta/var 5)})
  ;; [m/result '#{(= m/var 5) (= m/result 4)}]
  (meval '(= 5 5) '#{})
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
