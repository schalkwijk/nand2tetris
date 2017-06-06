(ns analyzer.parser)

(declare parse-subroutine-call)

(defn- in?
  "True if collection contains element"
  [collection element]
  (some #(= element %) collection))

(def built-in-type-keywords ["var" "int" "char" "boolean" "void"])
(def op-symbols ["+" "-" "*" "/" "&" "|" "<" ">" "="])
(def keyword-constants ["true" "false" "null" "this"])

(defn- raise [message]
  (throw (Exception. message)))

(defn- format-token-for-error [token]
  (str (:type token) " with value " (:value token)))

(defn- consume [type {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= (:type next-token) type)
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type ", got " (format-token-for-error next-token))))))

(defn- consume-identifier-or-built-in-type [{:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (or (and (= (:type next-token) :keyword)
                 (in? built-in-type-keywords (:value next-token)))
            (= (:type next-token) :identifier))

      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected identifier or built-in type, got " (format-token-for-error next-token))))))

(defn- consume-matching-value [type value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= next-token {:type type :value value})
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type " with value " value ", got " (format-token-for-error next-token))))))

(defn- optionally-consume-matching-value [type value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= next-token {:type type :value value})
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      {:tokens tokens :parsed-elements parsed-elements})))

(defn- optionally-consume-period-and-identifier [{:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)
        next-next-token (nth tokens 1)]
    (if (and (= next-token {:type :symbol :value "."})
             (= (:type next-next-token) :identifier))
      {:tokens (drop 2 tokens) :parsed-elements (conj parsed-elements next-token next-next-token)}
      {:tokens tokens :parsed-elements parsed-elements})))

(defn- looking-at-keywords [potential-keywords tokens]
  (let [next-token (first tokens)]
    (and (= (:type next-token :keyword))
         (in? potential-keywords (:value next-token)))))

(defn- looking-at-symbols [potential-symbols tokens]
  (let [next-token (first tokens)]
    (and (= (:type next-token :symbol))
         (in? potential-symbols (:value next-token)))))

(defn- looking-at [type tokens]
  (let [next-token (first tokens)]
    (= (:type next-token) type)))

(defn- looking-at-symbol [value tokens]
  (let [next-token (first tokens)]
    (and (= (:value next-token) value)
         (= (:type next-token) :symbol))))

(defn- looking-at-identifier-and-symbols [symbols tokens]
  (let [next-token (first tokens)
        next-next-token (nth tokens 1)]
    (and (= (:type next-token) :identifier)
         (and (= (:type next-next-token) :symbol)
              (in? symbols (:value next-next-token))))))

(defn- combine-under-attribute [attribute old-state new-state]
  {:tokens (:tokens new-state)
   :parsed-elements (conj (:parsed-elements old-state) {attribute (:parsed-elements new-state)})})

(defn- optionally-parse-more-var-decs [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol "," tokens)
    (->> {:tokens tokens :parsed-elements parsed-elements}
         (consume :symbol)
         (consume :identifier)
         optionally-parse-more-var-decs)
    {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-class-var-dec [tokens]
  (->> {:tokens tokens :parsed-elements []}
       (consume :keyword) ;; field / static
       (consume-identifier-or-built-in-type) ;; variable type
       (consume :identifier) ;; variable name
       (optionally-parse-more-var-decs)
       (consume-matching-value :symbol ";")))

(defn- parse-subroutine-var-dec [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol ";" tokens)
    (consume-matching-value :symbol ";" {:tokens tokens :parsed-elements parsed-elements})
    (->> {:tokens tokens :parsed-elements parsed-elements}
         (consume :identifier) ;; variable name
         (optionally-consume-matching-value :symbol ",") ;; optional comma
         recur)))

(defn- optionally-parse-subroutine-var-dec [{:keys [tokens parsed-elements]}]
  (if (not (looking-at-keywords ["var"] tokens))
    {:tokens tokens :parsed-elements parsed-elements}
    (->> {:tokens tokens :parsed-elements []}
         (consume-matching-value :keyword "var")
         (consume-identifier-or-built-in-type)
         (parse-subroutine-var-dec)
         (combine-under-attribute :varDec {:parsed-elements parsed-elements})
         recur)))

(defn- parse-term [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at :integerConstant tokens)
    (consume :integerConstant {:tokens tokens :parsed-elements parsed-elements})

    (looking-at :stringConstant tokens)
    (consume :stringConstant {:tokens tokens :parsed-elements parsed-elements})

    (looking-at-keywords keyword-constants tokens)
    (consume :keyword {:tokens tokens :parsed-elements parsed-elements})

    (looking-at-identifier-and-symbols ["(" "."] tokens)
    (parse-subroutine-call {:tokens tokens :parsed-elements parsed-elements})

    :else (consume :identifier {:tokens tokens :parsed-elements parsed-elements})))

(defn- parse-expression [{:keys [tokens parsed-elements]}]
  (let [expression-with-term
        (->> {:tokens tokens :parsed-elements []}
             (parse-term)
             (combine-under-attribute :term {:parsed-elements parsed-elements}))]

    (if (not (looking-at-symbols op-symbols (:tokens expression-with-term)))
      expression-with-term
      (recur (consume :symbol expression-with-term)))))

(defn- parse-expression-list [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol ")" tokens)
    {:tokens tokens :parsed-elements parsed-elements}
    (->> {:tokens tokens :parsed-elements []}
         (parse-expression)
         (combine-under-attribute :expression {:parsed-elements parsed-elements})
         (optionally-consume-matching-value :symbol ",")
         recur)))

(defn- optionally-parse-expression-list [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol ")" tokens)
    {:tokens tokens :parsed-elements parsed-elements}
    (->> {:tokens tokens :parsed-elements []}
         parse-expression-list
         (combine-under-attribute :expressionList {:parsed-elements parsed-elements}))))

(defn- parse-subroutine-call [{:keys [tokens parsed-elements]}]
  (->> {:tokens tokens :parsed-elements parsed-elements}
       (consume :identifier)
       (optionally-consume-period-and-identifier)
       (consume-matching-value :symbol "(")
       (optionally-parse-expression-list)
       (consume-matching-value :symbol ")")))

(defn- parse-subroutine-statements [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at-keywords ["do"] tokens)
    (->> {:tokens tokens :parsed-elements []}
         (consume-matching-value :keyword "do")
         parse-subroutine-call
         (consume-matching-value :symbol ";")
         (combine-under-attribute :doStatement {:parsed-elements parsed-elements}))

    (looking-at-keywords ["let"] tokens)
    {:tokens tokens :parsed-elements parsed-elements}

    (looking-at-keywords ["if"] tokens)
    {:tokens tokens :parsed-elements parsed-elements}

    (looking-at-keywords ["while"] tokens)
    {:tokens tokens :parsed-elements parsed-elements}

    (looking-at-keywords ["return"] tokens)
    {:tokens tokens :parsed-elements parsed-elements}

    :else {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-subroutine-body [{:keys [tokens parsed-elements]}]
  (->> {:tokens tokens :parsed-elements []}
       (consume-matching-value :symbol "{")
       (optionally-parse-subroutine-var-dec)
       (parse-subroutine-statements)
       (consume-matching-value :symbol "}")
       (combine-under-attribute :subroutineBody {:parsed-elements parsed-elements})))

(defn- parse-function-arguments [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol ")" tokens)
    {:tokens tokens :parsed-elements parsed-elements}
    (->> {:tokens tokens :parsed-elements parsed-elements}
        (optionally-consume-matching-value :symbol ",") ;; optional comma
        (consume-identifier-or-built-in-type) ;; variable type
        (consume :identifier) ;; variable name
        recur)))

(defn- optionally-parse-function-arguments [{:keys [tokens parsed-elements]}]
  (if (looking-at-symbol ")" tokens)
    {:tokens tokens :parsed-elements parsed-elements}
    (->> {:tokens tokens :parsed-elements []}
         parse-function-arguments
         (combine-under-attribute :parameterList {:parsed-elements parsed-elements}))))

(defn- parse-subroutine [tokens]
  (->> {:tokens tokens :parsed-elements []}
       (consume :keyword) ;; subroutine type
       (consume-identifier-or-built-in-type) ;; return type
       (consume :identifier) ;; method name
       (consume-matching-value :symbol "(")
       (optionally-parse-function-arguments)
       (consume-matching-value :symbol ")")
       (parse-subroutine-body)))

(defn- parse-class-body [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at-keywords ["constructor" "function" "method"] tokens)
    (recur (combine-under-attribute :subroutineDec {:parsed-elements parsed-elements} (parse-subroutine tokens)))

    (looking-at-keywords ["static" "field"] tokens)
    (recur (combine-under-attribute :classVarDec {:parsed-elements parsed-elements} (parse-class-var-dec tokens)))

    :else {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-class [{:keys [tokens parsed-elements]}]
  (let [cross-roads (->> {:tokens tokens :parsed-elements []}
                         (consume-matching-value :keyword "class")
                         (consume :identifier)
                         (consume-matching-value :symbol "{"))]

    (if (looking-at-keywords ["constructor" "function" "method" "static" "field"] (:tokens cross-roads))
      (->> cross-roads
           parse-class-body
           (consume-matching-value :symbol "}")
           (combine-under-attribute :class {:parsed-elements parsed-elements}))
      (raise "class body does not start with subroutine or variable declaration"))))

(defn parse-tokens [tokens]
  (let [starting-state {:tokens tokens :parsed-elements []}]
    (cond
      (looking-at-keywords ["class"] tokens)
      (:parsed-elements (parse-class starting-state))

      (looking-at-keywords ["method" "function" "constructor" "field" "static"] tokens)
      (:parsed-elements (parse-class-body starting-state))

      (looking-at-symbol "{" tokens)
      (:parsed-elements (parse-subroutine-body starting-state)))))
