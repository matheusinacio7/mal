(ns mal.reader)

(defn tokenize [str]
  (let [regex-expr #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"
        matches (re-seq regex-expr str)]
    (->> matches
         (map (fn [[_ captured]] captured))
         (filter #(not= "" %)))))

(defn string-integer? [str]
  (try
    (Integer/parseInt str)
    true
    (catch NumberFormatException _
      false)))

(defn read-atom [tokens]
  (let [token (first tokens)
        remaining (rest tokens)
        atom (cond
               (string-integer? token) {:value (Integer/parseInt token), :type :number}
               (= "" token) (:type :eof)
               :else {:name token :type :symbol})]
    [atom remaining]))

(declare read-form)

(defn read-list [tokens]
  (loop [current-list {:type :list :children []}
         remaining (rest tokens)]
    (let [next-token (first remaining)]
      (cond
        (= next-token ")") [current-list (rest remaining)]
        (not-empty remaining) (let [[child remaining] (read-form remaining)]
                                (recur (update current-list :children #(conj % child))
                                       remaining))
        :else (throw (ex-info "Unmatched (" {:type "Interpretation error"}))))))

(defn read-form [tokens]
  (case (first tokens)
    "(" (read-list tokens)
    (read-atom tokens)))

(defn read-forms [tokens]
  (loop [forms []
         remaining tokens]
    (if (empty? remaining)
      forms
      (let [[new-form new-remaining] (read-form remaining)]
        (recur (conj forms new-form) new-remaining)))))

(defn read-str [str]
  (-> str
      tokenize
      read-forms))
