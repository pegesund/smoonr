(ns lang.tests
  (:use clojure.test)
  (:require [lang.structures :as s]
            [lang.search :as search]
            )
  )

(deftest meaning-of-life (is 42 42))

(deftest test-phrase-search
  "test if a phrase if found in compressed string"
  (let [wc (s/create-word-counters)
        f (s/create-field wc)
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
        f (s/create-field wc)
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


(deftest test-search
  "Try or search"
  (let [wc (s/create-word-counters)
        f (s/create-field wc)
        s1 "This is Petter writing. Petter writes a lot. Petter should drink less."
        s2 "Petter lives in Norway."
        s3 "This string contains rubbish"
        res (transient {})
        res2 (transient {})
        ]
    (println "Starting test-or-search")
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



(run-tests 'lang.tests)
