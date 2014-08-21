(ns lang.search
  ; (:use clojure.structures)
  (:require [lang.structures :as s])
  (:import (cern.colt.list.tint IntArrayList))
  )
    
(defn find-phrase [index str-ids]
  "Find phrase in the index. Lookfor is a seq of word-idx"
  (let  [word_a (:word index)
         num_a (:num index)
         pos_a (:pos index)
         start_a (:start index)
         word_a_size (.size ^IntArrayList word_a)
         mother-id (first str-ids)
         mother-pos (.binarySearchFromTo ^IntArrayList word_a mother-id 0 word_a_size)
         ]
    (if (< mother-pos 0)
      false
      (let [mother-num (.get ^IntArrayList num_a mother-pos)
            mother-start (.get ^IntArrayList start_a mother-pos)
            ]
        (loop [mother-ndx 0]
          (if (= mother-ndx mother-num)
            false
            (let [res
                  (let [mother-txt-pos (.get ^IntArrayList pos_a (+ mother-start mother-ndx))]
                    (loop [lookfor (rest str-ids)
                           lookfor-txt-pos (inc mother-txt-pos)
                           ]
                      (if (empty? lookfor)
                        true
                        (let [lookfor-child (first lookfor)
                              child-pos (.binarySearchFromTo ^IntArrayList word_a lookfor-child 0 word_a_size)]
                          (if (< child-pos 0)
                            false
                            (let [child-num (^int .get ^IntArrayList num_a child-pos)
                                  child-start (^int .get ^IntArrayList start_a child-pos)
                                  child-pos-found (.binarySearchFromTo ^IntArrayList pos_a lookfor-txt-pos child-start (+ child-start child-num)) ]
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


(defmacro forColt [[[i val] colt-arr] & code]
  `(let [finish# (.size ~(with-meta colt-arr {:tag 'IntArrayList}))]
     (loop [ ~i 0]
       (let [~val (.getQuick ~(with-meta colt-arr {:tag 'IntArrayList}) ~i)]
         (when (< ~i finish#)
           ~@code
           (recur (inc ~i)))))))


(defmacro forColtResult [[[i val] start-res colt-arr] & code]
  "Use like this: (forColtResult [[i val] {100 101} colt-arr] [i val])"
  `(let [finish# (.size ~(with-meta colt-arr {:tag 'IntArrayList}))
         res# (transient ~start-res)
         ]
     (loop [ ~i 0]
       (let [~val (.getQuick ~(with-meta colt-arr {:tag 'IntArrayList}) ~i)]
         (if (< ~i finish#)
           (do
             (assoc! res# (first ~@code) (second ~@code))
             (recur (inc ~i))
             ) 
           (persistent! res#)
             )))))



;(defn join-colt-colt [c1 c2]
;  (let [
;  (forColt [[i val] ]
;           (
