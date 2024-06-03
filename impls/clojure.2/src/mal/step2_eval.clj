(ns mal.step2-eval
  (:gen-class) 
  (:require [mal.printer :as printer]
            [mal.reader :as reader]))

(def repl-env {"+" +
               "-" -
               "*" *
               "/" /})

(defn READ [in]
  (reader/read-str in))

(declare eval-form)

(defn EVAL [ast env]
  (map
   (fn [form]
     (cond
       (not= (:type form) :list) (eval-form form env)
       (empty? (:children form)) form
       :else (let [evald-form     (eval-form form env)
                   evald-children (:children evald-form)
                   fn             (first evald-children)
                   args           (rest evald-children)]
               (apply fn args))))
   ast))

(defn eval-form [form env]
  (case (:type form)
    :symbol (if-let [v (get env (:name form))]
              v
              (throw (ex-info "unbound symbol" {:type "unbound"})))
    :list {:type     :list
           :children (EVAL (:children form) env)}
    (:value form)))

(defn PRINT [in]
  (printer/print-evald in))

(defn rep [in]
  (-> in
      READ
      (#(EVAL % repl-env))
      PRINT))

(defn -main
  ([] (-main ""))
  ([_] (loop []
         (print "user> ")
         (flush)
         (-> (read-line)
             (#(if (nil? %)
                 (do
                   (println)
                   (println "bye!")
                   (System/exit 0))
                 %))
             (#(try
                 (rep %)
                 (catch Exception e
                   (println (ex-message e)))))
             println)
         (recur))))
