(ns lang.lucene
  (:require [clucy.core :as clucy])
  (:import (java.io StringReader File)
           (org.apache.lucene.analysis Analyzer TokenStream)
           (org.apache.lucene.analysis.standard StandardAnalyzer)
           (org.apache.lucene.document Document Field Field$Index Field$Store)
           (org.apache.lucene.index IndexWriter IndexReader Term
                                    IndexWriterConfig DirectoryReader FieldInfo)
           (org.apache.lucene.queryparser.classic QueryParser)
           (org.apache.lucene.search BooleanClause BooleanClause$Occur
                                     BooleanQuery IndexSearcher Query ScoreDoc
                                     Scorer TermQuery)
           (org.apache.lucene.search.highlight Highlighter QueryScorer
                                               SimpleHTMLFormatter)
           (org.apache.lucene.util Version AttributeSource)
           (org.apache.lucene.store NIOFSDirectory RAMDirectory Directory))
)

(def ^{:dynamic true} *version* Version/LUCENE_CURRENT)
(def ^{:dynamic true} *analyzer* (StandardAnalyzer. *version*))

(defn- index-writer
  "Create an IndexWriter."
  ^IndexWriter
  [index]
  (IndexWriter. index
                (IndexWriterConfig. *version* *analyzer*)))

(defonce index (clucy/disk-index "/var/tmp/wiki"))
(defonce writer (index-writer index))

(defn close-index [index]
  (.close index)
 )

(defn commit-index [index]
  (.commit index)
)


(defn- map-stored
  "Returns a hash-map containing all of the values in the map that
  will be stored in the search index."
  [map-in]
  (merge {}
         (filter (complement nil?)
                 (map (fn [item]
                        (if (or (= nil (meta map-in))
                                (not= false
                                      (:stored ((first item) (meta map-in)))))
                          item)) map-in))))



(defn- concat-values
  "Concatenate all the maps values being stored into a single string."
  [map-in]
  (apply str (interpose " " (vals (map-stored map-in)))))


(defn- add-field
  "Add a Field to a Document.
  Following options are allowed for meta-map:
  :stored - when false, then do not store the field value in the index.
  :indexed - when false, then do not index the field.
  :analyzed - when :indexed is enabled use this option to disable/eneble Analyzer for current field.
  :norms - when :indexed is enabled user this option to disable/enable the storing of norms."
  ([document key value]
     (add-field document key value {}))

  ([document key value meta-map]
     (.add ^Document document
           (Field. (clucy/as-str key) (clucy/as-str value)
                   (if (false? (:stored meta-map))
                     Field$Store/NO
                     Field$Store/YES)
                   (if (false? (:indexed meta-map))
                     Field$Index/NO
                     (case [(false? (:analyzed meta-map)) (false? (:norms meta-map))]
                       [false false] Field$Index/ANALYZED
                       [true false] Field$Index/NOT_ANALYZED
                       [false true] Field$Index/ANALYZED_NO_NORMS
                       [true true] Field$Index/NOT_ANALYZED_NO_NORMS))))))

(defn- map->document
  "Create a Document from a map."
  [map]
  (let [document (Document.)]
    (doseq [[key value] map]
      (add-field document key value (key (meta map))))
    (if true
      (add-field document :_content (concat-values map)))
    document))

(defn add-to-index [writer m]
  (.addDocument writer
                (map->document m))
)
  


(defn write-test [writer]
  (add-to-index writer {:name "Bob", :job "Builder"}))
  
