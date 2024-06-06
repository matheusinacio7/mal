(ns mal.env)

(defn create-env [outer]
  (atom {:outer outer
         :data {}}))

(defn env-set! [env sym value]
  (swap! env
         (fn [curr-env]
           (update curr-env :data #(assoc % sym value)))))

(defn env-find [env sym] 
  (if-not (nil? (get-in @env [:data sym]))
    env
    (when-let [outer (:outer @env)]
      (recur outer sym))))

(defn env-get [env sym]
  (if-let [match-env (env-find env sym)]
    (get-in @match-env [:data sym])
    (throw (Exception. (str sym " not found")))))
