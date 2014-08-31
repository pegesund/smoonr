(ns lang.rating
  (:use clojure.test)
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            )
  (:import (cern.colt.list.tint IntArrayList))
  )

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn tfidf [doc-id field word-id]
  (let [phrases @(:phrases field)
        ^IntArrayList p-index (get phrases doc-id)
        ^IntArrayList p-words (:word p-index)
        ^IntArrayList p-nums (:num p-index)
        p-size (.size p-words)
        p-pos (.binarySearchFromTo p-words word-id 0 (dec p-size))]
    (if (> 0 p-pos)
      (do (println "Not found in phrase") 0)
      (let [num-in-doc (.getQuick p-nums p-pos)
            num-of-terms-in-doc (:num-of-words-in-doc p-index)
            tf (double (/ num-in-doc num-of-terms-in-doc))
            num-docs @(:num-docs field)
            ^IntArrayList word-in-docs-arr (:word-in-docs field)
            num-of-docs-with-word (.getQuick word-in-docs-arr word-id)
            idf (Math/log (/ num-docs num-of-docs-with-word))
            tf-idf (* tf idf)]
        ; (println "--- tf: " tf " idf: " idf " tf-idf: " tf-idf)
        ; (println "doc-id: " doc-id " word-id: " word-id " num-in-doc" num-in-doc " doc-size" num-of-terms-in-doc)
        tf-idf)
      )
    )
  )
            
            
        

(defn calculate [hits word-fields]
  "Calculates score (so far only tfidf) on a parse/search.
   Words comes as id's and fields as strings"
  (let [p-queue (atom {})
       fw-objects (for [w word-fields]
                    [(get @s/all-fields (first w)) (second w)])]
    (doseq [doc-id hits]
      (let [rating-list-pr-word 
            (loop [l fw-objects
                   acc []]
              (if (empty? l)
                acc
                (recur
                 (rest l)
                 (conj acc (tfidf doc-id (first (first l)) (second (first l))))
                 ))
              )
            rating (reduce 
                    + 
                    rating-list-pr-word
                    )]
        ; (println "Word-weights: " rating-list-pr-word)
        ; (println "Handling doc-id: " doc-id "rating: " rating " class: " (class rating) " list: " rating-list-pr-word)
        (swap! p-queue assoc doc-id rating)
        )
      )
    @p-queue
    )
  )
  
                           
(defn search-and-score [query]
  (let [res (parse/search query)
        hits (first res)
        words (second res)]
    ; (println "Got hits: " hits " and words: " words)
    (calculate hits words)
    )
)


 

