(ns mal.step4-if-fn-do
  (:gen-class) 
  (:require [mal.env :as env]
            [mal.printer :as printer]
            [mal.reader :as reader]
            [mal.core :as core]))

(def repl-env (env/create-env nil))
(doseq [[sym value] mal.core/mal-ns]
  (env/env-set! repl-env sym value))

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
      
      "do" (let [forms (butlast (rest children))
                 last-form (last children)]
             (doseq [inner-form forms]
               (eval-form inner-form env))
             (eval-form last-form env))
      
      "if" (let [condition (eval-form (nth children 1) env)
                 then (nth children 2)
                 else (nth children 3 nil)]
             (if (#{false nil} condition)
               (eval-form then env)
               (when (not (nil? else))
                 (eval-form else env))))
      
      "fn*" (let [binds (nth children 1)
                  body (nth children 2)]
              {:type :function, :binds binds, :body body}))))

(defn apply-fn [form env args]
  (let [fn-list {:type :list
                 :children [{:type :symbol :name "let*"}
                            {:type :list
                             :children (into [] (interleave (get-in form [:binds :children]) args))}
                            (:body form)]}]
    (eval-form fn-list env)))

(defn apply-clj-fn [form env args]
  (let [fn (:fn form)
        evald-args (doall (map #(eval-form % env) args))]
    (apply fn evald-args)))

(defn eval-list [list env]
  (cond
    (not= (:type list) :list) (eval-form list env)
    (empty? (:children list)) list
    (special-forms (get-in list [:children 0 :name])) (eval-special-form list env)
    :else (let [children (:children list)
                fn       (eval-form (first children) env)
                args     (rest children)]
            (case (:type fn)
              :function (apply-fn fn env args)
              :clojure-function (apply-clj-fn fn env args)))))

(defn eval-form [form env]
  (case (:type form)
    :symbol (env/env-get env (:name form))
    :list (eval-list form env)
    form))

(defn READ [in]
  (reader/read-str in))

(defn EVAL [ast env]
  (let [do-form {:type :symbol
                 :name "do"}
        new-ast {:type :list
                 :children (into [do-form] ast)}]
    (eval-form new-ast env)))

(defn PRINT [in]
  (printer/print-ast in))

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
