(ns switch
  (:require [clojure.java.io :as io] [clojure.string :as str]))

(defn help-instructions-switch []
  (println "idiot switch: change what HEAD points to\n")
  (println "Usage: idiot switch [-c] <branch>\n")
  (println "Arguments:")
  (println "   -c   create the branch before switching to it"))

(defn switch_ref [working_dir db ref]
  (spit  (str working_dir "/" db "/HEAD") (str "ref: refs/heads/" ref "\n"))
  (println (str "Switched to branch '" ref "'")))

(defn create_new_ref [working_dir db ref]
  (let [head_content (slurp (io/file (str working_dir "/" db "/HEAD")))]
    (cond (= 41 (count head_content)) (spit (io/file (str working_dir "/" db "/refs/heads/" ref)) (slurp (io/file (str working_dir "/" db "/HEAD"))))
          :else (spit (io/file (str working_dir "/" db "/refs/heads/" ref)) (slurp (io/file (str working_dir "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_dir "/" db "/HEAD"))) 5))))))))
  (spit  (str working_dir "/" db "/HEAD") (str "ref: refs/heads/" ref "\n"))
  (println (str "Switched to a new branch '" ref "'")))

(defn switch [current_dir db content]
  (let [c_switch (first content)
        branch_content (rest content)
        c_branch (first branch_content)
        switch_branch (first content)
        length (count content)]
    (cond (or (= 0 (compare c_switch "--help")) (= 0 (compare c_switch "-h"))) (help-instructions-switch)
          (empty? c_switch) (println "Error: you must specify a branch name.")
          (not (.exists (io/file (str current_dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (= 0 (compare c_switch "-c")) (cond (> (count branch_content) 1) (println "Error: you may only specify one branch name.")
                                              (.exists (io/file (str current_dir "/" db "/refs/heads/" c_branch))) (println "Error: a ref with that name already exists.")
                                              (not (.exists (io/file (str current_dir "/" db "/refs/heads/" c_branch)))) (create_new_ref current_dir db c_branch))
          (not (= 0 (compare switch_branch "-c"))) (cond (> length 1) (println "Error: you may only specify one branch name.")
                                                         (not (.exists (io/file (str current_dir "/" db "/refs/heads/" switch_branch)))) (println "Error: no ref with that name exists.")
                                                         :else (switch_ref current_dir db c_switch)))))