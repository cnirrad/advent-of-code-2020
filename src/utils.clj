(ns utils
  (:require [clojure.java.io :as io]))

(defn read-file
  ([f]
   (read-file f "\n"))

  ([f split-on]
   (-> (io/resource f)
       slurp
       (.split split-on))))

(defmacro dbg [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
