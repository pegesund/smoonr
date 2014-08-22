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


(defmacro forColtResult [[[i val] start-res colt-arr] & code]
  "Use like this: (forColtResult [[i val] (transient {100 101}) colt-arr] [i val])"
  `(let [finish# (.size ~(with-meta colt-arr {:tag 'IntArrayList}))
         res# ~start-res
         ]
     (loop [ ~i 0]
       (let [~val (.getQuick ~(with-meta colt-arr {:tag 'IntArrayList}) ~i)]
         (if (< ~i finish#)
           (do
             (let [exec# ~@code] (when exec# (assoc! res# (first exec#) (second exec#))))
             (recur (inc ~i))
             ) 
           res#
             )))))

            
(defn find-phrase-str [field str]
"Find str in a field"
  (let [word_ids (s/string-to-ids str)]
    (find-phrase field word_ids)))


(defn find-all-docs-with-id-arr [field word_id]
  "Find all docs where word-id appear. Returns a record of type DocExist"
  (get @(:docs field) word_id)
)
      
      

(defn find-all-docs-with-id [field word_id]
  "Find all docs where word-id appear. Returns a transient hash"
  (let [res (get @(:docs field) word_id)]
    (if-not res
      {}
      (let [nums (:num res)]
        (forColtResult [[i val] (transient {}) (:doc res)]
                        [val (.getQuick ^IntArrayList nums i)]
                        ) 
        )
      )
    )
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

(defn q-or [field start-hash word-ids]
  "Find all docs and where all words exist.
   start-hash is a transient hash containing word - val, can be an transient({}) if there is no starting-point
  "
    (loop [rest-words word-ids]
      (if (empty? rest-words)
        start-hash
        (let [word-id (first rest-words)
              matching (find-all-docs-with-id-arr field word-id)
              matching-doc (:doc matching)
              matching-num (:num matching)
              ]
          (forColtResult [[i doc-id] start-hash matching-doc]
                         (let [num (.getQuick ^IntArrayList matching-num i)
                               old_num (get start-hash doc-id)]
                           (if old_num
                             [doc-id (+ num old_num)]
                             [doc-id num])
                           )
                         )
          (recur (rest rest-words))
          )
        )
      )
    )


                               
          
