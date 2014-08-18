(ns lang.tests
  (:use clojure.test)
  (:require [lang.structures :as s]
            [lang.search :as search]
            )
  )

(deftest meaning-of-life (is 42 42))

(deftest test-phrase-search
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

(run-tests 'lang.tests)
