(ns lang.search
  ; (:use clojure.structures)
  (:require [lang.structures :as s])
  )

(defn phrase-index [str]
  "Creates a fingerprint of a doc to be used in phrase-search
"
  (let [word_list (s/string-to-words (clojure.string/lower-case str))
        word_list_id (map #(get @s/words %) word_list)
        sorted_wordid (sort (distinct word_list_id))
        phrase_index (s/create-phrase-index)
        word_a (:word phrase_index)
        num_a (:num phrase_index)
        pos_a (:pos phrase_index)
        start_a (:start phrase_index)
        ]
    (doseq [id sorted_wordid]
      (let [positions (keep-indexed #(if (= %2 id) %1) word_list_id)]
        (println "id: " id " positions: " positions)
        (.add word_a id)
        (.add num_a (count positions))
        (.add start_a (.size pos_a))
        (doseq [pos positions] (.add pos_a pos))
        )
      )
    phrase_index
    )
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
