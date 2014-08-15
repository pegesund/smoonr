(ns lang.tests
  (:use clojure.test)
  (:require [lang.structures :as s])
  )

(deftest world-is-sane (is 8 8))

(deftest test-phrase-search
  (let [wc (s/create-word-counters)
        f (s/create-field wc)
        s "Dette er Petter som skriver. Petter bor i Norge. Norge er et land i nord"
        _a (s/add-string-to-field f s 100)
        index (s/phrase-index s)]
    (is (s/find-phrase-str index "Dette er"))
    (is (s/find-phrase-str index "Norge er"))
    (is (not (s/find-phrase-str index "Norge skriver")) "Should not pass")
    (is (s/find-phrase-str index "i nord"))
    )
  )

(run-tests 'lang.tests)
