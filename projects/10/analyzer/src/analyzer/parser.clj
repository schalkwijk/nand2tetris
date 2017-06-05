(ns analyzer.parser)

(defn- in?
  "True if collection contains element"
  [collection element]
  (some #(= element %) collection))

(def built-in-type-keywords ["var" "int" "char" "boolean" "void"])

(defn- raise [message]
  (throw (Exception. message)))

(defn- consume [type {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= (:type next-token) type)
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type ", got " (:type next-token) " with value " (:value next-token))))))

(defn- consume-identifier-or-built-in-type [{:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (or (and (= (:type next-token) :keyword)
                 (in? built-in-type-keywords (:value next-token)))
            (= (:type next-token) :identifier))

      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected identifier or built-in type, got " (:type next-token) " with value " (:value next-token))))))

(defn- consume-matching-value [type value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= next-token {:type type :value value})
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type " with value " value ", got " (:type next-token) " with value " (:value next-token))))))

(defn- optionally-consume-matching-value [type value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= next-token {:type type :value value})
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      {:tokens tokens :parsed-elements parsed-elements})))

(defn- looking-at-keywords [potential-keywords tokens]
  (let [next-token (first tokens)]
    (and (= (:type next-token :keyword))
         (in? potential-keywords (:value next-token)))))

(defn- looking-at-symbol [value tokens]
  (let [next-token (first tokens)]
    (and (= (:value next-token) value)
         (= (:type next-token) :symbol))))

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
    (let [subroutine-var-dec (->> {:tokens tokens :parsed-elements []}
                                  (consume-matching-value :keyword "var")
                                  (consume-identifier-or-built-in-type)
                                  (parse-subroutine-var-dec))]

      (optionally-parse-subroutine-var-dec {:tokens (:tokens subroutine-var-dec)
        :parsed-elements (conj parsed-elements {:varDec (:parsed-elements subroutine-var-dec)})}))))

(defn- parse-do-statement [{:keys [tokens parsed-elements]}]
  (combine-under-attribute
   :doStatement
   {:tokens tokens :parsed-elements parsed-elements}
   (->> {:tokens tokens :parsed-elements []}
        (consume-matching-value :keyword "do")
        (consume :identifier)
        (consume-matching-value :symbol ".")
        (consume :identifier)
        (consume-matching-value :symbol "(")
        (consume-matching-value :symbol ")")
        (consume-matching-value :symbol ";"))))

(defn- parse-subroutine-statements [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at-keywords ["do"] tokens)
    (parse-do-statement {:tokens tokens :parsed-elements parsed-elements})

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
  (let [subroutine-body (->> {:tokens tokens :parsed-elements []}
                             (consume-matching-value :symbol "{")
                             (optionally-parse-subroutine-var-dec)
                             (parse-subroutine-statements)
                             (consume-matching-value :symbol "}"))]
    {:tokens (:tokens subroutine-body)
     :parsed-elements (conj parsed-elements {:subroutineBody (:parsed-elements subroutine-body)})}))

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
    (let [function-arguments (parse-function-arguments {:tokens tokens :parsed-elements []})]
      {:tokens (:tokens function-arguments)
       :parsed-elements (conj parsed-elements {:parameterList (:parsed-elements function-arguments)})})))

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
    (let [subroutine (parse-subroutine tokens)]
      (recur {:tokens (:tokens subroutine) :parsed-elements
              (conj parsed-elements {:subroutineDec (:parsed-elements subroutine)})}))

    (looking-at-keywords ["static" "field"] tokens)
    (let [class-var-dec (parse-class-var-dec tokens)]
      (recur {:tokens (:tokens class-var-dec) :parsed-elements
              (conj parsed-elements {:classVarDec (:parsed-elements class-var-dec)})}))

    :else {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-class [{:keys [tokens parsed-elements]}]
  (let [cross-roads (->> {:tokens tokens :parsed-elements []}
                         (consume-matching-value :keyword "class")
                         (consume :identifier)
                         (consume-matching-value :symbol "{"))]

    (if (looking-at-keywords ["constructor" "function" "method" "static" "field"] (:tokens cross-roads))
      (let [class-elements (consume-matching-value :symbol "}" (parse-class-body cross-roads))]
        {:tokens (:tokens class-elements) :parsed-elements
         (conj parsed-elements {:class (:parsed-elements class-elements)})})
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
