(ns lang.search
  ; (:use clojure.structures)
  (:require [lang.structures :as s])
  )
    
(defn find-phrase [index str-ids]
  "Find phrase in the index. Lookfor is a seq of word-idx"
  (let  [word_a (:word index)
         num_a (:num index)
         pos_a (:pos index)
         start_a (:start index)
         word_a_size (.size word_a)
         mother-id (first str-ids)
         mother-pos (.binarySearchFromTo word_a mother-id 0 word_a_size)
         ]
    (if (< mother-pos 0)
      false
      (let [mother-num (.get num_a mother-pos)
            mother-start (.get start_a mother-pos)
            ]
        (loop [mother-ndx 0]
          (if (= mother-ndx mother-num)
            false
            (let [res
                  (let [mother-txt-pos (.get pos_a (+ mother-start mother-ndx))]
                    (loop [lookfor (rest str-ids)
                           lookfor-txt-pos (inc mother-txt-pos)
                           ]
                      (if (empty? lookfor)
                        true
                        (let [lookfor-child (first lookfor)
                              child-pos (.binarySearchFromTo word_a lookfor-child 0 word_a_size)]
                          (if (< child-pos 0)
                            false
                            (let [child-num (.get num_a child-pos)
                                  child-start (.get start_a child-pos)
                                  child-pos-found (.binarySearchFromTo pos_a lookfor-txt-pos child-start (+ child-start child-num)) ]
                              (if (< child-pos-found 0)
                                false
                                (recur (rest lookfor) (inc lookfor-txt-pos)))))))))]
              (if res res (recur (inc mother-ndx)))
              )
            )
          ) ; end mother loop
        )
      )
    )        
  )         
            
(defn find-phrase-str [field str]
"Find str in a field"
  (let [word_ids (s/string-to-ids str)]
    (find-phrase field word_ids)))


(defn find-all-docs-with-id [field word_id]
  "Find all docs where word-id appear"
  (let [res (get @(:docs field) word_id)]
        (or res []))
  )



(defn find-all-docs-with-word [field str]
  "Finds all docs where the word appear. The string is unparsed"
  (let [s (clojure.string/lower-case str)
        word_id (get @s/words s)]
        (find-all-docs-with-id field word_id)))


(defn join-results [d1 d2]
  "Joins two DocExist-records.
   If a word is fould in d2 it is increased in d1
   otherwise the word is added to d1"
  (let [doc1 (:doc d1)
        doc2 (:doc d2)
        num1 (:num d1)
        num2 (:num d2)
        size_1 (.size doc1)
        size_2 (.size doc2)
        d3 (.clone doc1)]
    (dotimes [i size_2] 
      (let [word_nr (.getQuick doc2 i)
            word_val (.getQuick num2 i)
            pos (.binarySearchFromTo d3 word_nr 0 size_1)]
        (println "Pos " pos)
        )
      )
    )
  )
    
   
