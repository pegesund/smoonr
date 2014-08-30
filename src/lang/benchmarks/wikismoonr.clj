(ns lang.benchmarks.wikismoonr
  (:use [clojure.data.xml])
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            )
  (:gen-class)
)



(defn fix-string [s]
  (-> s
      (.replaceAll "http:.* " "")
      (.replaceAll "\\{\\|.*?\\|\\}" " ")
      (.replaceAll "\\{\\{.*?\\}\\}" " ")
      (.replaceAll "\\{.*?\\}" " ")
      ; (.replaceAll "\\[\\[.*?\\]\\]" " ")
      (.replaceAll  "\n" " ")
      (.replaceAll "[|\\(\\),.*!\\[\\]@?=:$&'\"]" " ")
      (.replaceAll "<.*?>" "")
      (.replaceAll "\\s+" " ")
      )
)

; (defonce index (clucy/disk-index "/var/tmp/wiki"))
; (defonce writer (index-writer index))


(def counter (atom 0))
(def last-title (atom ""))
(defonce doc-id (atom 0))
(def timer (atom (quot (System/currentTimeMillis) 1000)))

(defn filter-tag [t a]  (filter #(= t (:tag %)) a))

(defn page-seq [rdr title-field content-field]
  (doseq [e
          (->> (parse rdr)
              :content
              ; (take 4)
              )]
    (when (= (:tag e) :page)
      (try
           (let [title (fix-string (doall (->> e :content first :content first)))
                 text (fix-string (fix-string (doall (->> e  :content  (filter-tag :revision) first :content (filter-tag :text) first :content first ))))]
             ; (clojure.pprint/pprint title)
             ; (clojure.pprint/pprint text)
             (s/add-string-to-field title-field title @doc-id)
             (s/add-string-to-field content-field text @doc-id)
             (swap! doc-id inc)
             (swap! counter #(+ % 1))
             (swap! last-title #(or title %))
             (when (= 0 (mod @counter 1000))
               (let [new-time (quot (System/currentTimeMillis) 1000)
                     time-diff (- new-time @timer)]
                 (println @counter " - " time-diff)
                 (reset! timer (quot (System/currentTimeMillis) 1000))
                 ) ; end let
               )
             )
           (catch Exception _e
             (println _e)
             )
           )
      )
  @counter
  )
)

(defn parsefile []
  (let [wc (s/create-word-counters)
        title-field (s/create-field "title" wc)
        content-field (s/create-field "content" wc)]
    (with-open [rdr (clojure.java.io/reader "/var/downloads/nowiki-latest-pages-articles-multistream.xml")]
      (page-seq rdr title-field content-field)
      )
    )
  )
    
  (defn -main [& args]
    (parsefile)
    )




