(ns mal.step1-read-print
  (:gen-class))

(defn READ [in]
  in)

(defn EVAL [in]
  in)

(defn PRINT [in]
  in)

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
