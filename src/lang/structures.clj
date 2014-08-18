(ns lang.structures
  ; (:require [cern.colt.list :as colt])
)


(defn create-doc-ndx []
  "Creates a treeMap which contains all documents in a collection. Doc-id which is the key is a string"
  (new java.util.TreeMap)
)

(def split-string #"[,.; ]")

(def doc-id-creator (atom 0))
(def word-id-creator (atom 0))
(defonce words (atom {}))

(defn add-new-word [word new-word-id]
  "Adds a new word to word hash"
  (swap! words #(assoc % word new-word-id)))


(defrecord WordCounters
    [ word-total word-in-docs ]
)


(defn create-word-counters []
  "Creates word-counters.
   word-total in the number of times a word has occured
   word-in-docs is the number of docs a word has appeared in
  "
  (WordCounters.
   (new cern.colt.list.tlong.LongArrayList)
   (new cern.colt.list.tlong.LongArrayList)
   )
  )

(defrecord Field
    [ docs word-total word-in-docs num-words num-words-unique num-docs]
)

(defn create-field [word-counter]
  "Creates a Field-record.
   The colt-arrays in the word-counters are stored directely in the records, due to speed.
   A Field record contains:
    - docs of type a treemap of colt-arrays, each array is as long as there are number of words
    - word-total a colt-array where idx is word number and val is number of occurences
    - word-in-docs a colt-array where idx is word number and val is number of docs where this word occure
    - number of words in the field
    - number of words unique  (length of word-map)
    - number of docs
  "
  (let [word-total (:word-total word-counter)
        word-in-docs (:word-in-docs word-counter)]
  (Field.
   (atom {})
   word-total
   word-in-docs
   (atom 0)
   (atom 0)
   (atom 0)
   )
  )
)


(defrecord DocExist
    [ doc num ]
)
    
(defn create-doc-exist [ ]
  "create two colt-arrays containing word-ndx and word-counter"  
  (DocExist. (new cern.colt.list.tint.IntArrayList) (new cern.colt.list.tint.IntArrayList))
)

(defn add-sorted-to-doc-exist [ doc-exist word-ndx num ]
  (let [arr-doc (:doc doc-exist)
        arr-num (:num doc-exist)
        arr-size (.size arr-doc)]
    (if (= 0 arr-size)
      (do (.add arr-doc word-ndx) (.add arr-num num))
      (let [found-pos (.binarySearchFromTo arr-doc word-ndx 0 (dec arr-size))]
        (if (> 0 found-pos)
          (let [ins-pos (dec (Math/abs found-pos))]
            (.beforeInsert arr-doc ins-pos word-ndx)
            (.beforeInsert arr-num ins-pos num)
            )
            (.setQuick arr-num found-pos (+ num (.getQuick arr-num found-pos)))
          )
        )
      )
    )
  )


(defn safe-inc-num-words [colt-array ndx inc-val]
  "Increases colt array at pos ndx with inc-val. Ensures that capacity is enough, fills array with zero if there are gaps in "
  (try
    (let [old-val (.get colt-array ndx)]
      (.set colt-array ndx (+ old-val inc-val)))
    (catch Exception _e
      (do (dotimes [_ (- ndx (.size colt-array))] (.add colt-array 0))
          (.add colt-array inc-val)))
    )
)

(defn add-word-to-field [field word num-occure doc-id]
  "Adds a word to a field. I word does not exist it is created.
   Num-occure is the number of times the word occures in the document
   This function has side-effects on the colt-arrays and are not mutable
  "
  (let [word-map (:word-map field)
        val (get @words word)]
    (swap! (:num-words field) inc)
    (let [ndx 
          (or val 
              (let [new-word-id (dec (swap! word-id-creator inc))]
                (add-new-word word new-word-id)
                (swap! (:num-words-unique field) inc)
                new-word-id))]
      (safe-inc-num-words (:word-total field) ndx num-occure)
      (safe-inc-num-words (:word-in-docs field) ndx 1)
      (let [docs-all (:docs field)
            doc-exist (or (get @docs-all ndx)
                           (let [new-doc-e (create-doc-exist)]
                             (swap! docs-all #(assoc % ndx new-doc-e))
                             new-doc-e))]
        (add-sorted-to-doc-exist doc-exist doc-id num-occure))
      ndx
      )
    )
  )

(defn count-words [items]
  "Creates a map of words where the number of words are aggregated and counted"
  (persistent!
   (reduce #(let [lowcase (.toLowerCase #^String %2)]
	      (assoc! %1 lowcase (inc (%1 lowcase 0)))) (transient {}) items)))

(defn string-to-words [str]
  "Splits a string into tokens, does not keep empty strings"
  (let [s_lc (clojure.string/lower-case str)]
    (filter #(not= "" %)  (clojure.string/split s_lc split-string))))

(defn string-to-ids [str]
  "Maps a string to word-ids. Not known id's are substituted with -1 - an impossible number"
  (map #(let [w (get @words %)] (if w w -1))
       (string-to-words str)))

(defn string-to-map [str]
  (count-words (string-to-words str)))


(defn add-string-to-field [field doc docid]
  "Main function to add a doc/str to a field. It adds word to global-word hash if needed"
  (let [wordlist (string-to-map doc)]
    (doseq [[word word-num] wordlist]
      (add-word-to-field field word word-num docid)
      )
    ))


(defrecord Doc
    [ words num phrase ]
)
    
(defn create-doc [ words num phrase ]
  "Creates a doc where all arguments should be of type colt-array.
   words are a sortes array of word num, num is the number of this word in a doc
   phrase is reserved for phrase-searn"
  (Doc. words num phrase)
)
 
(defn create-doc-vector [doc]
  "Creates a doc vector, returns a doc-record"
  (let [wordlist (string-to-map doc)
        wordlist_number    (map #(conj []
                                       (get @words (key %))
                                       (val %))
                                (seq wordlist))
        wordlist_sorted (sort-by first wordlist_number)
        colt_words (new cern.colt.list.tint.IntArrayList)
        colt_num (new cern.colt.list.tint.IntArrayList)
        ]
    (doseq [[wordid num] wordlist_sorted]
      (.add colt_words wordid)
      (.add colt_num num)
      )
    
 (create-doc colt_words colt_num nil)
 )
)
 
(defrecord PhraseIndex 
    [word num start pos]
)

(defn create-phrase-index []
  "word - an colt array of word id
   num  - an colt array of number of ocurs of this word
   start - start position of num words
   pos - positions, referenced by start and nums
   "
  (PhraseIndex.
        (new cern.colt.list.tint.IntArrayList)
        (new cern.colt.list.tint.IntArrayList)
        (new cern.colt.list.tint.IntArrayList)
        (new cern.colt.list.tint.IntArrayList)
        )
  )

(defn phrase-index [str]
  "Creates a fingerprint of a doc to be used in phrase-search
"
  (let [word_list (string-to-words (clojure.string/lower-case str))
        word_list_id (map #(get @words %) word_list)
        sorted_wordid (sort (distinct word_list_id))
        phrase_index (create-phrase-index)
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
  (let [word_ids (string-to-ids str)]
    (find-phrase field word_ids)))

  
                     
                     
