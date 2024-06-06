(ns mal.printer)

(defn print-ast
  ([ast] (print-ast nil ast))
  ([final-str ast]
   (let [ast-seq (if (seq? ast) ast [ast])
         node (first ast-seq)
         remaining (rest ast-seq)
         node-str (case (:type node)
                    :symbol (:name node)
                    :number (str (:value node))
                    :list (str "("
                               (if (empty? (:children node))
                                 ""
                                 (print-ast final-str (:children node)))
                               ")")
                    :boolean (:value node)
                    :nil "nil"
                    :function "#<function>")
         new-final-str (if (nil? final-str)
                         node-str
                         (str final-str " " node-str))]
     (if (empty? remaining)
       new-final-str
       (recur new-final-str remaining)))))

(defn print-evald
  [ast]
  (cond
    (and (= (:type ast) :list)
         (empty? (:children ast))) "()"
    (= (:type ast) :function) "#<function>"
    (nil? ast) "nil"
    :else ast))
