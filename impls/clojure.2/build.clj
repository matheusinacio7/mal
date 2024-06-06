(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'heyset/step4_if_fn_do)
(def version (format "1.2.%s" (b/git-count-revs nil)))
(def class-dir "dist/classes")
(def uber-file (format "dist/%s-standalone.jar" (name lib)))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "dist"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile '[mal.step4-if-fn-do]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'mal.step4-if-fn-do}))
