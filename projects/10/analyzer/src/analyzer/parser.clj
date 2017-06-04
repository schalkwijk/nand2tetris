(ns analyzer.parser)

(defn- in?
  "True if collection contains element"
  [collection element]
  (some #(= element %) collection))

(defn- raise [message]
  (throw (Exception. message)))

(defn- consume [type {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= (:type next-token) type)
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type ", got " (:type next-token) " with value " (:value next-token))))))

(defn- consume-identifier-or-keyword [keyword-value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (or (= next-token {:type :keyword :value keyword-value})
            (= (:type next-token) :identifier))
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type ", got " (:type next-token) " with value " (:value next-token))))))

(defn- consume-matching-value [type value {:keys [tokens parsed-elements]}]
  (let [next-token (first tokens)]
    (if (= next-token {:type type :value value})
      {:tokens (rest tokens) :parsed-elements (conj parsed-elements next-token)}
      (raise (str "Expected " type " with value " value ", got " (:type next-token) " with value " (:value next-token))))))

(defn- parse-class-var-dec [] nil)

(defn- looking-at-keyword [potential-keywords tokens]
  (let [next-token (first tokens)]
    (and (= (:type next-token :keyword))
         (in? potential-keywords (:value next-token)))))

(defn- parse-subroutine [tokens]
  (->> {:tokens tokens :parsed-elements []}
       (consume :keyword) ;; subroutine type
       (consume-identifier-or-keyword "void") ;; return type
       (consume :identifier) ;; method name
       (consume-matching-value :symbol "(")
       (consume-matching-value :symbol ")")
       (consume-matching-value :symbol "{")
       (consume-matching-value :symbol "}")))

(defn- parse-class-body [{:keys [tokens parsed-elements]}]
  (cond
    (looking-at-keyword ["constructor" "function" "method"] tokens)
    (let [subroutine (parse-subroutine tokens)]
      (recur {:tokens (:tokens subroutine) :parsed-elements
              (conj parsed-elements {:subroutineDec (:parsed-elements subroutine)})}))

    (looking-at-keyword ["static" "field"] tokens) (parse-class-var-dec)
    :else {:tokens tokens :parsed-elements parsed-elements}))

(defn- parse-class [tokens]
  (let [cross-roads (->> {:tokens tokens :parsed-elements []}
                         (consume-matching-value :keyword "class")
                         (consume :identifier)
                         (consume-matching-value :symbol "{"))]

    (if (looking-at-keyword ["constructor" "function" "method" "static" "field"] (:tokens cross-roads))
      (:parsed-elements (consume-matching-value :symbol "}" (parse-class-body cross-roads)))
      (raise "class body does not start with subroutine or variable declaration"))))

(defn- handle-class [tokens]
  [{:class (parse-class tokens)}])

(defn parse-tokens [tokens]
  (handle-class tokens))
