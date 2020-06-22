(ns init
  (:require [clojure.java.io :as io]))

(defn help-instructions-init []
  (println "idiot init: initialize a new database\n")
  (println "Usage: idiot init\n")
  (println "Arguments:")
  (println "   -h   print this message"))

(defn create_dirs [dir db]
  (println "Initialized empty Idiot repository in .git directory")
  (.mkdir (io/file  (str dir "/" db)))
  (.mkdir (io/file  (str dir "/" db "/objects")))
  (.mkdir (io/file (str dir "/" db "/refs")))
  (.mkdir (io/file (str dir "/" db "/refs/heads")))
  (spit  (str dir "/" db "/HEAD") "ref: refs/heads/master\n"))

(defn init [dir db content]

  (let [instruction (first content)]
    (cond (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-init)
          (true? (= nil instruction)) (cond (.exists (io/file (str dir "/" db))) (println "Error: .git directory already exists")
                                            :else (create_dirs dir db))
          :else (println
                 "Error: init accepts no arguments"))))