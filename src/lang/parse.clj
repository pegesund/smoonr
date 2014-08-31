(ns lang.parse
  (:use [instaparse.core]
        [clojure.core.match :only (match)]
        )
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            )
  (:import (cern.colt.list.tint IntArrayList))
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

(defn plogic [p1 p2 operator current-field words-acc]
  (case operator
    :and (clojure.set/intersection (search-tree p1 current-field words-acc) (search-tree p2 current-field words-acc))
    :or (clojure.set/union (search-tree p1 current-field words-acc) (search-tree p2 current-field words-acc))
    :not-ok
    )
  )


(defn phrase-and [words current-field]
  "Finds all occurences of where all the words occurs in a field"
  (let [field (get @s/all-fields current-field)]
    (if-not field
      (throw (Exception. (str "This field is not defined: " current-field)))
      (let [word-ids (keep #(get @s/words (second %)) (rest words))
            start-and (search/find-all-docs-with-id field (first word-ids))]
        (loop [acc start-and
               l (rest word-ids)
               ]
          (if (empty? l)
            [acc word-ids field]
            (let [new-docs (search/find-all-docs-with-id field (first l))
                  and-docs (clojure.set/intersection acc new-docs)]
              (recur and-docs (rest l))
              )
            )
          )
        )
      )
    )
  )

(defn phrase [words current-field words-acc]
  (let [[and-docs word-ids field] (phrase-and words current-field)
        field (get @s/all-fields current-field)
        phrases (:phrases field)
        res (filter #(search/find-phrase 
                  (get phrases %)
                  word-ids) and-docs)]
    (doseq [word-id word-ids] (conj! words-acc [current-field word-id]))
    (into #{} res)
    )
)

(defn pword [word current-field words-acc]
  (let [field (get @s/all-fields current-field)]
    (if-not field
      (throw (Exception. (str "This field is not defined: " current-field)))
      (let [word-id (get @s/words word)]
        (conj! words-acc [current-field word-id])
        (search/find-all-docs-with-id field word-id)
        )
      )
    )
)



(defn search-tree [tree current-field words-acc]
  "Executes a search based on a parsed tree-structure"
  (let [left (first tree)]
    (cond
     (= left :field-sentences) (search-tree (first (rest tree)) nil words-acc)
     (= left :field-sentence) (search-tree (rest tree) nil words-acc)
     (safe-test (= (first left) :field-name)) (search-tree (first (rest tree)) (second (first tree)) words-acc)
     (or (= left :logic-bind) (= left :field-logic-bind))
         (let [p1 (nth tree 1)
               operator (first (nth tree 2))
               p2 (nth tree 3)]
           (plogic p1 p2 operator current-field words-acc)
           )
     (= :phrase left) (phrase tree current-field words-acc)
     (= :word left) (pword (second tree) current-field words-acc)
     :else (println "No match and world falls apart: " tree)
     )
    )
)
         
(defn search [query]
  (let [tree (parse-query query)
        words-acc (transient [])
        ]
    [(search-tree tree nil words-acc) (persistent! words-acc)]
    )
  )

; try with (clojure.pprint/pprint (parse-query "field:\"xxx  vvv\" OR bbb AND (fto:yyy AND bbb OR xx:vvv)"))

