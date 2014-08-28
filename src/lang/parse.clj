(ns lang.parse
  (:use [instaparse.core]
        [clojure.core.match :only (match)]
        )
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            )
  (:gen-class :main true)
  )

(set! *warn-on-reflection* false)
(set! *unchecked-math* true)

(def parse-query
  (parser
   "
    field-sentences = (field-sentence|field-logic-bind|paren-bind) (paren-bind|field-logic-bind)*
    field-logic-bind = field-sentence space logic space (field-sentence|field-logic-bind)
    field-sentence = field-name field sentence space?
    <sentence> = (element|logic-bind|paren-bind) (paren-bind|logic-bind)*
    <paren-bind> = lparen (logic-bind|field-logic-bind|field-sentence) rparen
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
  (let [field (get @s/all-fields current-field)]
    (if-not field
      (throw (Exception. (str "This field is not defined: " current-field)))
      (let [word-id (get @s/words word)]
        (search/find-all-docs-with-id field word-id)
        )
      )
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
         
(defn search [query]
  (let [tree (parse-query query)]
    (search-tree tree nil)
    )
  )

; try with (clojure.pprint/pprint (parse-query "field:\"xxx  vvv\" OR bbb AND (fto:yyy AND bbb OR xx:vvv)"))

(defn minibench []
  (let [wc (s/create-word-counters)
        f1 (s/create-field "f1" wc)
        f2 (s/create-field "f2" wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less."
        s2 "Petter lives in Norway."
        s3 "This string contains rubbish"
        s4 "Petter is only a true believer when it comes to C"
        ]
    (s/add-string-to-field f1 s1 100)
    (s/add-string-to-field f1 s2 101)
    (s/add-string-to-field f1 s3 102)
    (s/add-string-to-field f2 s4 103)    
    (dotimes [_ 1000] (time (dotimes [_ 1000] (search "f1:petter OR (f2:petter OR (f2:rubbish AND f1:petter))"))))
    )
  )

(defn -main
  "The application's main function"
  [& args]
  (minibench)
  )
