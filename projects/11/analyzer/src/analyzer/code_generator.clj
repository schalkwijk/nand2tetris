(ns analyzer.code-generator
  (:require [analyzer.vm-command-writer :as writer]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.string :as str]))

(defn- zip-str [s]
  (zip/xml-zip (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn- consume-content-until-value [value zipper & consumed]
  (let [consumed (if consumed consumed [])
        current-node-content (zip/node (zip/down zipper))]
    (if (= value current-node-content)
      {:zipper zipper :consumed consumed}
      (recur value (zip/right zipper) (conj consumed current-node-content)))))

(defn- compile-do-statement [do-statement]
  (let [{zipper :zipper function-call :consumed} (consume-content-until-value "(" (zip/right do-statement))]
    [(writer/write-subroutine-call (str/join function-call) (count (zip/children zipper)))]))

(defn- compile-subroutine-body [commands subroutine-body]
  (let [statement (:tag (zip/node subroutine-body))]
    (cond
      (= statement :doStatement)
      (let [do-statement-commands
            (->> subroutine-body
                 zip/down ;; go into the do statement body
                 compile-do-statement
                 (concat commands))]
        (recur do-statement-commands (zip/next subroutine-body)))

      :else commands)))

(defn- compile-subroutine [class-name zipper]
  (let [subroutine-type (zip/node (zip/down zipper))
        subroutine-name (zip/node (zip/down (zip/right (zip/right zipper))))
        number-of-args (count (zip/children (zip/right (zip/right (zip/right (zip/right zipper))))))
        commands [(writer/write-subroutine-declaration subroutine-type class-name subroutine-name number-of-args)]]

    (->> zipper
         zip/right ;; return type
         zip/right ;; subroutine name
         zip/right ;; open paren
         zip/right ;; paremeter list
         zip/right ;; close paren
         zip/right ;; subroutine body
         zip/down ;; in to subroutine body
         zip/next ;; skip over {
         zip/next ;; go to statements
         zip/down ;; down into statements
         (compile-subroutine-body [])
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
  (let [parse-tree-zipper (zip-str parse-tree)
        class-name (zip/node (zip/down (zip/right (zip/down parse-tree-zipper))))
        class-body (zip/right (zip/right (zip/right (zip/down parse-tree-zipper))))]
    (compile-class class-name class-body [])))
