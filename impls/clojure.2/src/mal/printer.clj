(ns mal.printer 
  (:require [clojure.string :as str]))

(defn print-ast
  [ast]
  (case (:type ast)
     :symbol (:name ast)
             :number (str (:value ast))
             :list (str "("
                        (if (empty? (:children ast))
                          ""
                          (str/join " " (doall (map #(print-ast %) (:children ast)))))
                        ")")
             :boolean (:value ast)
             :nil "nil"
             :function "#<function>"
             nil ""))

(defn print-evald
  [ast]
  (cond
    (and (= (:type ast) :list)
         (empty? (:children ast))) "()"
    (= (:type ast) :function) "#<function>"
    (nil? ast) "nil"
    :else ast))
