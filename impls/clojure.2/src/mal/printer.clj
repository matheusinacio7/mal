(ns mal.printer 
  (:require [clojure.string :as str]))

(defn print-ast
  ([ast] (print-ast nil ast))
  ([final-str ast]
   (let [node (first ast)
         remaining (rest ast)
         node-str (case (:type node)
                    :symbol (:name node)
                    :number (str (:value node))
                    :list (str "("
                               (if (empty? (:children node))
                                 ""
                                 (print-ast final-str (:children node)))
                               ")"))
         new-final-str (if (nil? final-str)
                         node-str
                         (str final-str " " node-str))]
     (if (empty? remaining)
       new-final-str
       (recur new-final-str remaining)))))

(defn print-evald
  [ast]
  (->> ast
       (map #(cond
               (and (= (:type %) :list)
                    (empty? (:children %))) "()"
               :else %))
       (str/join "\n")))
