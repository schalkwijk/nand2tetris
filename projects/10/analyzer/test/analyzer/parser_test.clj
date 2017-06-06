(ns analyzer.parser-test
  (:require  [analyzer.parser :refer :all]
             [clojure.test :refer :all]))

(defn- create-token [type value]
  {:type type :value value})

(def class-t (create-token :keyword "class"))
(def class-name (create-token :identifier "Main"))
(def open-curly (create-token :symbol "{"))
(def close-curly (create-token :symbol "}"))
(def open-square (create-token :symbol "["))
(def close-square (create-token :symbol "]"))
(def open-paren (create-token :symbol "("))
(def close-paren (create-token :symbol ")"))
(def method (create-token :keyword "method"))
(def fraction (create-token :identifier "Fraction"))
(def method-name (create-token :identifier "foo"))
(def int-t (create-token :keyword "int"))
(def void (create-token :keyword "void"))
(def method-parameter (create-token :keyword "y"))
(def semicolon (create-token :symbol ";"))
(def static (create-token :keyword "static"))
(def field (create-token :keyword "field"))
(def do (create-token :keyword "do"))
(def l-token (create-token :keyword "let"))
(def foo (create-token :identifier "foo"))
(def bar (create-token :identifier "bar"))
(def baz (create-token :identifier "baz"))
(def constant-string (create-token :stringConstant "CONSTANT"))
(def comma (create-token :symbol ","))
(def period (create-token :symbol "."))
(def divide (create-token :symbol "/"))
(def plus (create-token :symbol "+"))
(def equal-t (create-token :symbol "="))
(def constant (create-token :integerConstant "3"))
(def var (create-token :keyword "var"))
(def return (create-token :keyword "return"))

;; class Main { method Fraction foo() {} }
(deftest handling-function-declarations
  (let [tokens [class-t class-name open-curly method fraction method-name
                open-paren close-paren open-curly close-curly close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 0] class-t
      [0 :class 1] class-name
      [0 :class 2] open-curly
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] fraction
      [0 :class 3 :subroutineDec 2] method-name
      [0 :class 3 :subroutineDec 3] open-paren
      [0 :class 3 :subroutineDec 4] close-paren
      [0 :class 3 :subroutineDec 5 :subroutineBody 0] open-curly
      [0 :class 3 :subroutineDec 5 :subroutineBody 1] close-curly
      [0 :class 4] close-curly)))

;; class Main { method void foo() {} }
(deftest handling-void-function-returns
  (let [tokens [class-t class-name open-curly method void method-name
                open-paren close-paren open-curly close-curly close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] void
      [0 :class 3 :subroutineDec 2] method-name)))

;; class Main { method int foo() {} }
(deftest handling-built-in-type-function-returns
  (let [tokens [class-t class-name open-curly
                method int-t method-name open-paren close-paren open-curly close-curly
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] int-t
      [0 :class 3 :subroutineDec 2] method-name)))

;; class Main { method Fraction baz; static int foo, bar; }
(deftest handling-class-level-variable-declarations
  (let [tokens [class-t class-name open-curly
                field fraction baz semicolon
                static int-t foo comma bar semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :classVarDec 0] field
      [0 :class 3 :classVarDec 1] fraction
      [0 :class 3 :classVarDec 2] baz
      [0 :class 3 :classVarDec 3] semicolon
      [0 :class 4 :classVarDec 0] static
      [0 :class 4 :classVarDec 1] int-t
      [0 :class 4 :classVarDec 2] foo
      [0 :class 4 :classVarDec 3] comma
      [0 :class 4 :classVarDec 4] bar
      [0 :class 4 :classVarDec 5] semicolon)))

;; class Main { method int foo(int bar, Fraction baz) {} }
(deftest handling-built-in-type-function-returns
  (let [tokens [class-t class-name open-curly
                method int-t method-name open-paren
                int-t bar comma fraction baz
                close-paren open-curly close-curly
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :subroutineDec 4 :parameterList 0] int-t
      [0 :class 3 :subroutineDec 4 :parameterList 1] bar
      [0 :class 3 :subroutineDec 4 :parameterList 2] comma
      [0 :class 3 :subroutineDec 4 :parameterList 3] fraction
      [0 :class 3 :subroutineDec 4 :parameterList 4] baz)))

;; method int foo() { var int bar, baz; var Fraction foo; }
(deftest handling-function-variable-declarations
  (let [tokens [method int-t method-name open-paren close-paren open-curly
                var int-t bar comma baz semicolon
                var fraction foo semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineDec 5 :subroutineBody 0] open-curly
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 0] var
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 1] int-t
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 2] bar
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 3] comma
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 4] baz
      [0 :subroutineDec 5 :subroutineBody 1 :varDec 5] semicolon
      [0 :subroutineDec 5 :subroutineBody 2 :varDec 0] var
      [0 :subroutineDec 5 :subroutineBody 2 :varDec 1] fraction
      [0 :subroutineDec 5 :subroutineBody 2 :varDec 2] foo
      [0 :subroutineDec 5 :subroutineBody 2 :varDec 3] semicolon
      [0 :subroutineDec 5 :subroutineBody 3] close-curly)))

;; { do bar(); }
(deftest handling-simple-function-calls
  (let [tokens [open-curly do bar open-paren close-paren semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :doStatement 0] do
      [0 :subroutineBody 1 :doStatement 1] bar
      [0 :subroutineBody 1 :doStatement 2] open-paren
      [0 :subroutineBody 1 :doStatement 3] close-paren
      [0 :subroutineBody 1 :doStatement 4] semicolon)))

;; { do foo.bar(); }
(deftest handling-function-calls-on-objects
  (let [tokens [open-curly
                do foo period bar open-paren close-paren semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :doStatement 0] do
      [0 :subroutineBody 1 :doStatement 1] foo
      [0 :subroutineBody 1 :doStatement 2] period
      [0 :subroutineBody 1 :doStatement 3] bar
      [0 :subroutineBody 1 :doStatement 4] open-paren
      [0 :subroutineBody 1 :doStatement 5] close-paren
      [0 :subroutineBody 1 :doStatement 6] semicolon
      )))

;; { do foo.bar(baz / 3); }
(deftest handling-function-calls-with-simple-expressions
  (let [tokens [open-curly
                do foo period bar open-paren
                baz divide constant
                close-paren semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :doStatement 0] do
      [0 :subroutineBody 1 :doStatement 1] foo
      [0 :subroutineBody 1 :doStatement 2] period
      [0 :subroutineBody 1 :doStatement 3] bar
      [0 :subroutineBody 1 :doStatement 4] open-paren
      [0 :subroutineBody 1 :doStatement 5 :expressionList 0 :expression 0 :term 0] baz
      [0 :subroutineBody 1 :doStatement 5 :expressionList 0 :expression 1] divide
      [0 :subroutineBody 1 :doStatement 5 :expressionList 0 :expression 2 :term 0] constant
      [0 :subroutineBody 1 :doStatement 6] close-paren
      [0 :subroutineBody 1 :doStatement 7] semicolon)))

;; { do foo(bar(baz / 3)); }
(deftest handling-function-calls-within-function-calls
  (let [tokens [open-curly
                do foo open-paren
                baz open-paren baz divide constant close-paren
                close-paren semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :doStatement 0] do
      [0 :subroutineBody 1 :doStatement 1] foo
      [0 :subroutineBody 1 :doStatement 2] open-paren
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 0] baz
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 1] open-paren
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 2 :expressionList 0 :expression 0 :term 0] baz
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 2 :expressionList 0 :expression 1] divide
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 2 :expressionList 0 :expression 2 :term 0] constant
      [0 :subroutineBody 1 :doStatement 3 :expressionList 0 :expression 0 :term 3] close-paren
      [0 :subroutineBody 1 :doStatement 4] close-paren
      [0 :subroutineBody 1 :doStatement 5] semicolon
      [0 :subroutineBody 2] close-curly)))

;; { let foo = bar.baz("CONSTANT STRING"); }
(deftest handling-let-statements
  (let [tokens [open-curly l-token foo equal-t
                bar period baz open-paren constant-string close-paren
                semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :letStatement 0] l-token
      [0 :subroutineBody 1 :letStatement 1] foo
      [0 :subroutineBody 1 :letStatement 2] equal-t
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 0] bar
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 1] period
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 2] baz
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 3] open-paren
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 4 :expressionList 0 :expression 0 :term 0] constant-string
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 5] close-paren
      [0 :subroutineBody 1 :letStatement 4] semicolon
      [0 :subroutineBody 2] close-curly)))

;; { let foo[bar / 3] = baz; }
(deftest handling-let-statements-with-array-accessing-on-LHS
  (let [tokens [open-curly l-token foo open-square
                bar divide constant close-square equal-t baz
                semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :letStatement 0] l-token
      [0 :subroutineBody 1 :letStatement 1] foo
      [0 :subroutineBody 1 :letStatement 2] open-square
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 0] bar
      [0 :subroutineBody 1 :letStatement 3 :expression 1] divide
      [0 :subroutineBody 1 :letStatement 3 :expression 2 :term 0] constant
      [0 :subroutineBody 1 :letStatement 4] close-square
      [0 :subroutineBody 1 :letStatement 5] equal-t)))

;; { let foo = 3 + bar[baz]; }
(deftest handling-let-statements-with-array-accessing-on-LHS
  (let [tokens [open-curly l-token foo equal-t
                constant plus bar open-square baz close-square
                semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :letStatement 0] l-token
      [0 :subroutineBody 1 :letStatement 1] foo
      [0 :subroutineBody 1 :letStatement 2] equal-t
      [0 :subroutineBody 1 :letStatement 3 :expression 0 :term 0] constant
      [0 :subroutineBody 1 :letStatement 3 :expression 1] plus
      [0 :subroutineBody 1 :letStatement 3 :expression 2 :term 0] bar
      [0 :subroutineBody 1 :letStatement 3 :expression 2 :term 1] open-square
      [0 :subroutineBody 1 :letStatement 3 :expression 2 :term 2 :expression 0 :term 0] baz
      [0 :subroutineBody 1 :letStatement 3 :expression 2 :term 3] close-square
      [0 :subroutineBody 1 :letStatement 4] semicolon)))

;; { return; }
(deftest handling-return-with-no-expression
  (let [tokens [open-curly return semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :returnStatement 0] return
      [0 :subroutineBody 1 :returnStatement 1] semicolon)))

;; { return foo + bar; }
(deftest handling-return-with-expression
  (let [tokens [open-curly return
                foo plus bar
                semicolon close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :subroutineBody 1 :returnStatement 0] return
      [0 :subroutineBody 1 :returnStatement 1 :expression 0 :term 0] foo
      [0 :subroutineBody 1 :returnStatement 1 :expression 1] plus
      [0 :subroutineBody 1 :returnStatement 1 :expression 2 :term 0] bar
      [0 :subroutineBody 1 :returnStatement 2] semicolon)))

;; TODO
;; handle let with array on RHS
;; '(' expression ')' | unaryOp term
;; test for multiple method declarations in class
