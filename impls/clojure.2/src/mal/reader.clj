(ns mal.reader 
  (:require [clojure.string :as str]))

(defn tokenize [str]
  (let [regex-expr #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"
        matches (re-seq regex-expr str)]
    (->> matches
         (map (fn [[_ captured]] captured))
         (filter #(not= "" %)))))

(defn read-atom [atom]
  atom)

(defn read-list [list]
  list)

(defn read-form [form]
  form)

(defn read-forms [tokens]
  tokens)

(defn read-str [str]
  (-> str
      tokenize
      read-forms))

(tokenize "(  + 2   (*  3  4)  )  ")
