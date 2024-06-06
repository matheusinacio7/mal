(ns mal.core)

(def mal-ns {"+" {:type :clojure-function
                  :fn (fn [& args]
                        {:type :number
                         :value (apply + (doall (map :value args)))})}

             "-" {:type :clojure-function
                  :fn (fn [& args]
                        {:type :number
                         :value (apply - (doall (map :value args)))})}

             "*" {:type :clojure-function
                  :fn (fn [& args]
                        {:type :number
                         :value (apply * (doall (map :value args)))})}

             "/" {:type :clojure-function
                  :fn (fn [& args]
                        {:type :number
                         :value (apply / (doall (map :value args)))})}

             "not" "(fn* (cond)
                         (if cond false true))"})
