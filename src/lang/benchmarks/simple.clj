(ns lang.benchmarks.simple
  (:use [clojure.data.xml])
  (:require [lang.structures :as s]
            [lang.search :as search]
            [lang.parse :as parse]
            )
)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn test-set-speed [num]
  (loop [acc (transient #{})
         counter 0]
    (if (= counter num)
      (count (persistent! acc))
      (let [r (rand-int num)
            t (conj! acc r)]
        (recur t (inc counter))
        )
      )
    )
)

(defn java-set-speed [num]
  (loop [acc (new java.util.HashSet)
         counter 0]
    (if (= counter num)
      (count acc)
      (let [r (rand-int num)]
        (.add acc r)
        (recur acc (inc counter))
        )
      )
    )
)

(defn test-large-hash-speed [num]
  (let [acc (atom {})]
    (loop [counter 0]
    (if (= counter num)
      (count @acc)
      (let [r (rand-int num)
            val (take 1000 (repeatedly #(rand-int 1000)))]
        (swap! acc assoc r val)
        (recur (inc counter))
        )
      )
    )
    )
  )


(defn test-java-large-hash-speed [num]
  (let [^java.util.HashMap acc (new java.util.HashMap)]
    (loop [counter 0]
    (if (= counter num)
      (count @acc)
      (let [r (rand-int num)
            val (let [arr (new java.util.ArrayList)]
                  (dotimes [_ 10000] (.add arr 0)))]
        (.put acc r val)
        (recur (inc counter))
        )
      )
    )
    )
  )


(defn create-hash-speed [num]
 (loop [i num acc (transient #{})] 
   (if (= i 0)
     (count (persistent! acc))
     (recur (dec i) acc)
   )
   )
)   
