(ns mal.step4-if-fn-do
  (:gen-class) 
  (:require [mal.printer :as printer]
            [mal.reader :as reader]
            [mal.env :as env]))

(def repl-env (env/create-env nil))
(env/env-set! repl-env "+" +)
(env/env-set! repl-env "-" -)
(env/env-set! repl-env "*" *)
(env/env-set! repl-env "/" /)

(declare eval-form
         rep)

(def special-forms #{"def!" "let*" "do" "if" "fn*"})

(defn eval-special-form [form env]
  (let [children (:children form)
        form-name (get-in children [0 :name])]
    (case form-name
      "def!" (let [key   (get-in form [:children 1 :name])
                   value (eval-form (get-in form [:children 2]) env)]
               (env/env-set! env key value)
               value)
      "let*" (let [new-env  (env/create-env env)
                   bindings (partition 2 (get-in form [:children 1 :children]))
                   to-eval  (get-in form [:children 2])]
               (doseq [[key-form value] bindings]
                 (env/env-set! new-env (:name key-form) (eval-form value new-env)))
               (eval-form to-eval new-env))
      "do" (let [forms (butlast children)
                 last-form (last children)]
             (for [inner-form form]
               (eval-form inner-form forms))
             (eval-form last-form env)))))

(defn eval-list [list env]
  (cond
    (not= (:type list) :list) (eval-form list env)
    (empty? (:children list)) list
    (special-forms (get-in list [:children 0 :name])) (eval-special-form list env)
    :else (let [evald-children (doall (map #(eval-form % env) (:children list)))
                fn             (first evald-children)
                args           (rest evald-children)]
            (apply fn args))))

(defn eval-form [form env]
  (case (:type form)
    :symbol (env/env-get env (:name form))
    :list (eval-list form env)
    (:value form)))

(defn READ [in]
  (reader/read-str in))

(defn EVAL [ast env]
  (doall (map
          #(eval-form % env)
          ast)))

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
