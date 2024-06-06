(ns mal.core 
  (:require [mal.printer :as printer]))

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

             "not"    "(fn* (cond)
                         (if cond false true))"})

(into [] '(2 3 4))
