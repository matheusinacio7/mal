(ns mal.step3-env
  (:gen-class) 
  (:require [mal.printer :as printer]
            [mal.reader :as reader]
            [mal.env :as env]))

(def repl-env (env/create-env nil))
(env/env-set! repl-env "+" +)
(env/env-set! repl-env "-" -)
(env/env-set! repl-env "*" *)
(env/env-set! repl-env "/" /)

(defn READ [in]
  (reader/read-str in))

(declare eval-form)

(defn EVAL [ast env]
  (doall (map
          (fn [form]
            (cond
              (not= (:type form) :list) (eval-form form env)
              (empty? (:children form)) form
              (= (get-in form [:children 0 :name]) "def!") (let [key   (get-in form [:children 1 :name])
                                                                 value (eval-form (get-in form [:children 2]) env)]
                                                             (env/env-set! env key value)
                                                             value)
              (= (get-in form [:children 0 :name]) "let*") (let [new-env  (env/create-env env)
                                                                 bindings (partition 2 (get-in form [:children 1 :children]))
                                                                 to-eval  (get-in form [:children 2])]
                                                             (doseq [[key-form value] bindings]
                                                               (env/env-set! new-env (:name key-form) (eval-form value new-env)))
                                                             (eval-form to-eval new-env))
              :else (let [evald-children (doall (map #(eval-form % env) (:children form)))
                          fn             (first evald-children)
                          args           (rest evald-children)]
                      (apply fn args))))
          ast)))

(defn eval-form [form env]
  (case (:type form)
    :symbol (if-let [v (env/env-get env (:name form))]
              v
              (throw (ex-info "unbound symbol" {:type "unbound"})))
    :list (let [evald-children (doall (map #(eval-form % env) (:children form)))
                fn             (first evald-children)
                args           (rest evald-children)]
            (apply fn args))
    (:value form)))

(defn PRINT [in]
  (printer/print-evald in))

(defn rep [in]
  (-> in
      READ
      (#(EVAL % repl-env))
      PRINT))

(rep "(def! y (let* (z 7) z))")

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
