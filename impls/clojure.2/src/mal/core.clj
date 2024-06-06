(ns mal.core 
  (:require [mal.printer :as printer]))

(defn equal? [a b]
  (let [z (cond
            (not= (:type a) (:type b)) false
            (and (= (:type a) :list) (= (:type b) :list))  (let [as (:children a)
                                                                 bs (:children b)]
                                                             (if (not= (count as) (count bs))
                                                               false
                                                               (every? #(true? (:value %))
                                                                       (map
                                                                        (fn [i]
                                                                          (equal? (nth as i) (nth bs i)))
                                                                        (range (count as))))))
            :else (= (:value a) (:value b)))]
    {:type  :boolean
     :value z}))

(def mal-ns {"+"      {:type :clojure-function
                       :fn   (fn [& args]
                               {:type  :number
                                :value (apply + (doall (map :value args)))})}

             "-"      {:type :clojure-function
                       :fn   (fn [& args]
                               {:type  :number
                                :value (apply - (doall (map :value args)))})}

             "*"      {:type :clojure-function
                       :fn   (fn [& args]
                               {:type  :number
                                :value (apply * (doall (map :value args)))})}

             "/"      {:type :clojure-function
                       :fn   (fn [& args]
                               {:type  :number
                                :value (apply / (doall (map :value args)))})}

             "prn"    {:type :clojure-function
                       :fn   (fn [& x]
                               (println (printer/print-ast x))
                               {:type  :nil
                                :value nil})}

             "list"   {:type :clojure-function
                       :fn   (fn [& args]
                               {:type     :list
                                :children (into [] args)})}

             "list?"  {:type :clojure-function
                       :fn   (fn [x]
                               (let [y (if (= (:type x) :list)
                                         true
                                         false)]
                                 {:type  :boolean
                                  :value y}))}

             "empty?" {:type :clojure-function
                       :fn   (fn [x]
                               (let [y (empty? (:children x))]
                                 {:type  :boolean
                                  :value y}))}

             "count"  {:type :clojure-function
                       :fn   (fn [x]
                               {:type  :number
                                :value (count (:children x))})}

             "="      {:type :clojure-function
                       :fn   equal?}

             "not"    "(fn* (cond)
                         (if cond false true))"})
