(ns compiler.compiler
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [compiler.symbol-table :as st]
            [compiler.zip-helpers :refer :all]
            [compiler.vm-command-writer :as writer]))

(def zip-node-children [zip/node zip/xml-zip zip/down])

(declare compile-expression)
(declare compile-expression-list)
(declare compile-statements)

(def if-count (atom -1))
(def w-count (atom -1))

(defn- pop-into-variable [variable-name symbol-table]
  (let [{segment :scope position :position} (st/get-symbol-by-name variable-name symbol-table)]
    (writer/write-segment-pop (name segment) position)))

(defn- push-variable [variable-name symbol-table]
  (let [{segment :scope position :position} (st/get-symbol-by-name variable-name symbol-table)]
    (writer/write-segment-push (name segment) position)))

(defn- compile-subroutine-call [symbol-table zipper]
  (let [{zipper :zipper function-call :consumed} (consume-content-until-value "(" zipper)]
    (conj (vec (compile-expression-list symbol-table zipper)) ;; cast to vec so conj behaves correctly
          (writer/write-subroutine-call (str/join function-call) (count (zip/children zipper))))))

(defn- compile-term [symbol-table zipper]
  (let [type (:tag (zip/node zipper))
        content (zip/node (zip/down zipper))]
    (cond
      (= type :integerConstant)
      [(writer/write-constant-push content)]

      (and (= content "(") (= type :symbol))
      (compile-expression symbol-table (zip/down (zip/right zipper)) [])

      (and (= content "-") (= type :symbol))
      (concat (compile-expression symbol-table (zip/right zipper) []) [(writer/write-negation-operator)])

      (= type :identifier)
      (let [next-token (zip/right zipper)
            token (if next-token (zip/node (zip/down next-token)) "")]
        (if (or (= "(" token) (= "." token))
          (compile-subroutine-call symbol-table zipper)
          [(push-variable content symbol-table)]))

      :else zipper)))

(defn- compile-expression [symbol-table zipper commands]
  (if (nil? zipper)
    commands
    (let [type (:tag (zip/node zipper))]
      (cond
        (= type :term)
        (recur symbol-table (zip/right zipper) (concat commands (compile-term symbol-table (zip/down zipper))))

        (= type :symbol)
        (concat commands (compile-expression symbol-table (zip/right zipper) []) [(writer/write-operator (zip/node (zip/down zipper)))])

        :else commands))))

(defn- compile-expression-list [symbol-table zipper]
  (reduce #(concat %1 (compile-expression symbol-table (zip/down (zip/xml-zip %2)) [])) [] (zip/children zipper)))

(defn- compile-do-statement [symbol-table do-statement]
  (let [{zipper :zipper function-call :consumed} (consume-content-until-value "(" (zip/right do-statement))
        number-of-args (count (filter #(not (= "," (first (:content %)))) (zip/children zipper)))]
    (conj (vec (compile-expression-list symbol-table zipper)) ;; cast to vec so conj behaves correctly
          (writer/write-subroutine-call (str/join function-call)
                                        number-of-args)
          (str "pop temp 0")))) ;; if it's a do statement, we discard the return value

(defn- compile-let-statement [symbol-table l-statement]
  (let [{variable-name :value zipper :zipper} (zip-and-fetch-node-content l-statement [zip/right])
        {expression :value zipper :zipper} (zip-and-apply zipper [zip/right zip/right] [zip/down])]
    (concat (compile-expression symbol-table expression []) [(pop-into-variable variable-name symbol-table)])))

(defn- compile-return [symbol-table return-statement]
  (if (= 2 (count (zip/children return-statement))) ;; void return
    [(writer/write-constant-push 0) "return"]
    (conj (vec (compile-expression symbol-table (zip/down (zip/right (zip/down return-statement))) [])) "return")))

(defn- compile-if-statement [symbol-table if-statement]
  (let [{if-expression :value zipper :zipper} (zip-and-apply if-statement [zip/right zip/right] zip-node-children)
        if-expression-commands (compile-expression symbol-table if-expression [])

        current-if-count (swap! if-count inc)
        if-labels [(str "if-goto IF_TRUE" current-if-count)
                   (str "goto IF_FALSE" current-if-count)
                   (str "label IF_TRUE" current-if-count)]

        {if-body :value zipper :zipper} (zip-and-apply zipper (repeat 3 zip/right) zip-node-children)
        if-body-commands (compile-statements [] symbol-table if-body)

        else-labels [(str "goto IF_END" current-if-count) (str "label IF_FALSE" current-if-count)]
        end-labels [(str "label IF_END" current-if-count)]

        maybe-else (= "else" (zip/node (zip/down (zip/right (zip/right zipper)))))
        else-body-commands (if maybe-else (compile-statements [] symbol-table (:value (zip-and-apply zipper (repeat 4 zip/right) zip-node-children))) [])]

    (concat if-expression-commands if-labels if-body-commands else-labels else-body-commands end-labels)))

(defn- compile-while-statement [symbol-table w-statement]
  (let [{w-expression :value zipper :zipper} (zip-and-apply w-statement [zip/right zip/right] zip-node-children)
        w-expression-commands (compile-expression symbol-table w-expression [])

        current-while-count (swap! if-count inc)
        if-labels [(str "if-goto IF_TRUE" current-while-count)
                   (str "goto IF_FALSE" current-while-count)
                   (str "label IF_TRUE" current-while-count)]

        {w-body :value zipper :zipper} (zip-and-apply zipper (repeat 3 zip/right) zip-node-children)
        w-body-commands (compile-statements [] symbol-table w-body)]

    (concat [(str "label WHILE_EXP" current-while-count)]
            w-expression-commands
            ["not" (str "if-goto WHILE_END" current-while-count)]
            w-body-commands
            [(str "goto WHILE_EXP" current-while-count) (str "label WHILE_END" current-while-count)])))

(defn- compile-statements [commands symbol-table subroutine-body]
  (if (nil? subroutine-body)
    commands
    (let [statement (:tag (zip/node subroutine-body))]
     (cond
       (= statement :doStatement)
       (let [do-statement-commands
             (->> subroutine-body
                  zip/down ;; go into the do statement body
                  (compile-do-statement symbol-table)
                  (concat commands))]
         (recur do-statement-commands symbol-table (zip/right subroutine-body)))

       (= statement :returnStatement)
       (->> subroutine-body
            (compile-return symbol-table)
            (concat commands))

       (= statement :letStatement)
       (let [l-statement-commands
             (->> subroutine-body
                  zip/down ;; go into the let statement body
                  (compile-let-statement symbol-table)
                  (concat commands))]
         (recur l-statement-commands symbol-table (zip/right subroutine-body)))

       (= statement :ifStatement)
       (let [if-statement-commands
             (->> subroutine-body
                  zip/down ;; go into the if statement body
                  (compile-if-statement symbol-table)
                  (concat commands))]
         (recur if-statement-commands symbol-table (zip/right subroutine-body)))

       (= statement :whileStatement)
       (let [w-statement-commands
             (->> subroutine-body
                  zip/down ;; go into the while statement body
                  (compile-while-statement symbol-table)
                  (concat commands))]
         (recur w-statement-commands symbol-table (zip/right subroutine-body)))

       :else commands))))

(defn- compile-subroutine [class-name zipper]
  (let [{subroutine-type :value zipper :zipper} (fetch-node-content zipper)
        {subroutine-name :value zipper :zipper} (zip-and-fetch-node-content zipper [zip/right zip/right])

        {arguments :value zipper :zipper} (zip-and-apply zipper [zip/right zip/right] [zip/node  zip/xml-zip]) ;; isolate arg list
        symbol-table (st/create-table-for-expression-list arguments)
        zipper (zip-and-discard zipper [zip/right zip/right zip/down zip/next zip/next])

        {symbol-table :symbol-table zipper :zipper} (st/add-local-vars-to-table zipper symbol-table)
        local-var-count (st/get-scope-variable-count :local symbol-table)
        commands [(writer/write-subroutine-declaration subroutine-type class-name subroutine-name local-var-count)]]

    (->> zipper
         zip/down ;; go into statements
         (compile-statements [] symbol-table)
         (concat commands))))

(defn- compile-class [class-name class-zipper commands]
  (cond
    (= :subroutineDec (:tag (zip/node class-zipper)))
    (->> class-zipper
         zip/down
         (compile-subroutine class-name)
         (concat commands)
         (recur class-name (zip/right class-zipper)))

    :else commands))

(defn compile-code [parse-tree]
  (reset! if-count -1) ;; hack because repl shenanigans
  (reset! w-count -1) ;; hack because repl shenanigans
  (let [parse-tree-zipper (zip-str parse-tree)
        class-name (zip/node (zip/down (zip/right (zip/down parse-tree-zipper))))
        class-body (zip/right (zip/right (zip/right (zip/down parse-tree-zipper))))]
    (compile-class class-name class-body [])))
