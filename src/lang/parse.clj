(ns lang.parse
  (:use [instaparse.core]
        [clojure.core.match :only (match)]
        )
)


(def parse-query
  (parser
   "
    field-sentences = (field-sentence|field-logic-bind|paren-bind) (paren-bind|field-logic-bind)*
    field-logic-bind = field-sentence space logic space (field-sentence|field-logic-bind)
    field-sentence = field-name field sentence space?
    <sentence> = (element|logic-bind|paren-bind) (paren-bind|logic-bind)*
    <paren-bind> = lparen (logic-bind|field-logic-bind) rparen
    logic-bind = (element|paren-bind) space logic space (element|logic-bind|paren-bind)
    <element> = (token|paren-bind)
    <logic> = (and|or|not)
    <token> = (phrase|word)
    <space> = <#'\\s+'>
    word = #'\\p{javaLowerCase}+[0-9]*'
    field-name =  #'\\p{javaLowerCase}+[0-9]*'
    phrase = p word (space+ word)* p
    <p> = <'\\\"'>
    <field> = <#':'>
    and = <'AND'>
    or = <'OR'>
    not = <'NOT'>
    <lparen> = <'('>
    <rparen> = <')'>
   "
   )
)

(defmacro safe-test [code]
  `(try (if (~@code) true false)
        (catch Exception e# false)))

(declare search-tree)

(defn plogic [p1 p2 operator current-field]
  (case operator
    :and (clojure.set/intersection (search-tree p1 current-field) (search-tree p2 current-field))
    :or (clojure.set/union (search-tree p1 current-field) (search-tree p2 current-field))
    :not-ok
    )
  )


(defn phrase [words current-field]
  (println "Parsing phrase")
  #{10 11 1}
)

(defn pword [word current-field]
  (println "Parsing word: " word)
  (case word
    "vvv" #{1 2 3}
    "yyy" #{1}
    "bbb" #{2 3 4}
    )
)



(defn search-tree [tree current-field]
  "Executes a search based on a parsed tree-structure"
  (let [left (first tree)]
    (cond
     (= left :field-sentences) (search-tree (first (rest tree)) nil)
     (= left :field-sentence) (search-tree (rest tree) nil)
     (safe-test (= (first left) :field-name)) (search-tree (first (rest tree)) (second (first tree)))
     (or (= left :logic-bind) (= left :field-logic-bind)) (do
                                                            (let [p1 (nth tree 1)
                                                                  operator (first (nth tree 2))
                                                                  p2 (nth tree 3)]
                                                              (plogic p1 p2 operator current-field)
                                                              )
                                                            )
     (= :phrase left) (phrase ["extract later"] current-field)
     (= :word left) (pword (second tree) current-field)
     :else (println "No match and world falls apart: " tree)
     )
    )
)
         


; try with (clojure.pprint/pprint (parse-query "field:\"xxx  vvv\" OR bbb AND (fto:yyy AND bbb OR xx:vvv)"))
