(ns mal.step1-read-print
  (:gen-class) 
  (:require [mal.printer :as printer]
            [mal.reader :as reader]))

(defn READ [in]
  (reader/read-str in))

(defn EVAL [in]
  in)

(defn PRINT [in]
  (printer/print-ast in))

(defn rep [in]
  (-> in
      READ
      EVAL
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
        rep
        println)
    (recur))))
