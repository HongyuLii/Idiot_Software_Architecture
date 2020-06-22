(ns rev_parse
  (:require [clojure.java.io :as io] [clojure.string :as str]))

(defn help-instructions-rev []
  (println "idiot rev-parse: determine which commit a ref points to\n")
  (println "Usage: idiot rev-parse <ref>\n")
  (println "<ref> can be:")
  (println "- a branch name, like 'master'")
  (println "- literally 'HEAD'")
  (println "- literally '@', an alias for 'HEAD'"))

(defn rev-parse [current_dir db content]
  (let [ref (first content)
        length (count content)]
    (cond (or (= 0 (compare ref "--help")) (= 0 (compare ref "-h"))) (help-instructions-rev)
          (empty? ref) (println "Error: you must specify a branch name.")
          (not (.exists (io/file (str current_dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (> length 1) (println "Error: you must specify a branch name and nothing else.")
          (or (= 0 (compare ref "@")) (= 0 (compare ref "HEAD"))) (cond (= 41 (count (slurp (io/file (str current_dir "/" db "/HEAD"))))) (print (slurp (io/file (str current_dir "/" db "/HEAD"))))
                                                                        :else (print (slurp (io/file (str current_dir "/" db "/" (str/trim-newline (subs (slurp (io/file (str current_dir "/" db "/HEAD"))) 5)))))))
          (and (or (not (= 0 (compare ref "HEAD"))) (not (= 0 (compare ref "@")))) (not (.exists (io/file (str current_dir "/" db "/refs/heads/" ref))))) (println (str "Error: could not find ref named " ref "."))
          :else (print (slurp (io/file (str current_dir "/" db "/refs/heads/" ref)))))))