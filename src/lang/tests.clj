(ns lang.tests
  (:use clojure.test)
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            [lang.rating :as rate]
            )
  )

(deftest meaning-of-life (is (= 42 42)))

(deftest test-phrase-search
  "test if a phrase if found in compressed string"
  (let [wc (s/create-word-counters)
        f (s/create-field "f1" wc)
        s "Dette er Petter som skriver. Petter bor i Norge. Norge er et land i nord"
        _a (s/add-string-to-field f s 100)
        index (s/phrase-index s)]
    (is (search/find-phrase-str index "Dette er"))
    (is (search/find-phrase-str index "Norge er"))
    (is (not (search/find-phrase-str index "Norge skriver")) "Should not pass")
    (is (search/find-phrase-str index "i nord")) 
    )
  )

(deftest find-a-word-in-field
  "Find all occurences of a word in a field"
  (let [wc (s/create-word-counters)
        f (s/create-field "f2" wc)
        s1 "This is Petter writing. Petter writes a lot."
        s2 "Petter lives in Norway."]
    (s/add-string-to-field f s1 100)
    (s/add-string-to-field f s2 101)
    (let [word-id1 (get @s/words "petter")
          word-id2 (get @s/words "is")
          docs1 (search/find-all-docs-with-id f word-id1)
          docs2 (search/find-all-docs-with-id f word-id2)
          ]
          (is (= #{100 101} docs1))
          (is (= #{100} docs2))
          )
    )
  )


(deftest test-logical-search
  "Try or search"
  (let [wc (s/create-word-counters)
        f (s/create-field "f3" wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less."
        s2 "Petter lives in Norway."
        s3 "This string contains rubbish"
        ]
    (s/add-string-to-field f s1 100)
    (s/add-string-to-field f s2 101)
    (s/add-string-to-field f s3 102)
    (let [word-id1 (get @s/words "petter")
          word-id2 (get @s/words "lives")
          word-id3 (get @s/words "lot")
          docs1 (search/logic-query f word-id1 word-id2 :or)
          docs2 (search/logic-query f word-id1 word-id2 :and)
          ]
      (is (= docs1 #{100 101}))
      (is (= #{101} docs2))
      )
    )
  )

(defn search [str]
  (first (parse/search str)))

(deftest test-query-parser
  "Try or search"
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
    (let [word-id1 (get @s/words "petter")
          word-id2 (get @s/words "lives")
          word-id3 (get @s/words "lot")
          word-id4 (get @s/words "only")
          docs1 (search "f2:petter")
          docs2 (search "f2:petter AND only")
          docs3 (search "f2:petter OR lives")
          docs4 (search "f2:petter AND lot")
          docs5 (search "f1:petter")
          docs6 (search "f1:petter AND f2:petter")
          docs7 (search "f1:petter OR f2:petter")
          docs8 (search "f2:petter OR rubbish")
          docs9 (search "f1:petter OR (f2:petter AND rubbish)")
          docs10 (search "f1:petter OR (f2:petter OR rubbish)")
          docs11 (search "f1:petter OR (f2:petter OR (f2:rubbish AND f1:petter))")
          ]
      (is (= #{103} docs1))
      (is (= #{103} docs2))
      (is (= #{103} docs3))
      (is (= #{} docs4))
      (is (= #{101 100} docs5))
      (is (= #{} docs6))
      (is (= #{100 101 103} docs7))
      (is (= #{103} docs8))
      (is (= #{103}))
      (is (= #{100 101 103}))
      (is (= #{100 101 103} docs11))
      )
    )
  )




(deftest test-phrase-parser
  "Test out phrase searches in parsed queries"
  (let [wc (s/create-word-counters)
        p1 (s/create-field "p1" wc)
        p2 (s/create-field "p2" wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less."
        s2 "Petter lives in Norway."
        s3 "This string contains rubbish"
        s4 "Petter is only a true believer when it comes to C. Petter writes nonesense."
        ]
    (s/add-string-to-field p1 s1 100)
    (s/add-string-to-field p1 s2 101)
    (s/add-string-to-field p2 s3 102)
    (s/add-string-to-field p2 s4 103)
    (let [word-id1 (get @s/words "petter")
          word-id2 (get @s/words "lives")
          word-id3 (get @s/words "writes")
          word-id4 (get @s/words "only")]
      (is (= #{} (search "p1:\"petter only\"")))
      (is (= #{100} (search "p1:\"petter writes\"")))
      (is (= #{100 103} (search "p1:\"petter writes\" OR p2:\"petter writes nonsense\"")))
      (is (= #{100 101 103} (search "p1:petter OR (p1:\"petter writes\" OR p2:\"petter writes nonsense\")")))
      )
    )
  )


(deftest test-tfidf
  "Test simple tf-idf algorithm"
  (let [wc (s/create-word-counters)
        f (s/create-field "p1" wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less. Writing is fun. Writing is here. A lot is done."
        s2 "Petter is only a true believer when it comes to C. Petter writes nonesense."
        s3 "Petters last sentence"
        ]
    (s/add-string-to-field f s1 100)
    (s/add-string-to-field f s2 101)
    (s/add-string-to-field f s3 102)
    (let [word-id1 (get @s/words "writing")
          word-id2 (get @s/words "lot")
          word-id3 (get @s/words "petter")]
      (let [r1 (rate/tfidf 100 f word-id1)
            r2 (rate/tfidf 100 f word-id2)
            r3 (rate/tfidf 100 f word-id3)]
        (is (> r1 r2 r3))
        )
      )
    )
  )


(deftest test-score
  "Test simple tf-idf algorithm"
  (let [wc (s/create-word-counters)
        f (s/create-field "f23" wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less. Writing is fun. Writing is here. A lot is done."
        s2 "Petter is only a true believer when it comes to C. Petter writes nonesense. Happy."
        s3 "Petters last sentence"
        ]
    (s/add-string-to-field f s1 100)
    (s/add-string-to-field f s2 101)
    (s/add-string-to-field f s3 102)
    (rate/search-and-score "score:petter AND writes AND should")
    )
)


(run-tests 'lang.tests)
