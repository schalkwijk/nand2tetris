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

(defn- looking-at-keyword [potential-keywords tokens]
  (let [next-token (first tokens)]
    (and (= (:type next-token :keyword))
         (in? potential-keywords (:value next-token)))))

(defn- looking-at-symbol [value tokens]
  (let [next-token (first tokens)]
    (and (= (:value next-token) value)
         (= (:type next-token) :symbol))))

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
       (consume-matching-value :symbol "{")
       (consume-matching-value :symbol "}")))

(defn- parse-class-body [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at-keyword ["constructor" "function" "method"] tokens)
    (let [subroutine (parse-subroutine tokens)]
      (recur {:tokens (:tokens subroutine) :parsed-elements
              (conj parsed-elements {:subroutineDec (:parsed-elements subroutine)})}))

    (looking-at-keyword ["static" "field"] tokens)
    (let [class-var-dec (parse-class-var-dec tokens)]
      (recur {:tokens (:tokens class-var-dec) :parsed-elements
              (conj parsed-elements {:classVarDec (:parsed-elements class-var-dec)})}))

    :else {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-class [{:keys [tokens parsed-elements]}]
  (let [cross-roads (->> {:tokens tokens :parsed-elements []}
                         (consume-matching-value :keyword "class")
                         (consume :identifier)
                         (consume-matching-value :symbol "{"))]

    (if (looking-at-keyword ["constructor" "function" "method" "static" "field"] (:tokens cross-roads))
      (let [class-elements (consume-matching-value :symbol "}" (parse-class-body cross-roads))]
        {:tokens (:tokens class-elements) :parsed-elements
         (conj parsed-elements {:class (:parsed-elements class-elements)})})
      (raise "class body does not start with subroutine or variable declaration"))))

(defn parse-tokens [tokens]
  (let [starting-state {:tokens tokens :parsed-elements []}]
    (cond
      (looking-at-keyword ["class"] tokens)
      (:parsed-elements (parse-class starting-state))

      (looking-at-keyword ["method" "function" "constructor" "field" "static"] tokens)
      (:parsed-elements (parse-class-body starting-state)))))
