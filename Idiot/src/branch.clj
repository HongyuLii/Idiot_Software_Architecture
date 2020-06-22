(ns branch
  (:require [clojure.java.io :as io]))

(defn help-instructions-branch []
  (println "idiot branch: list or delete branches\n")
  (println "Usage: idiot branch [-d <branch>]\n")
  (println "Arguments:")
  (println "   -d <branch>   delete branch <branch>"))

(defn compare_d [x]
  (= 0 (compare "-d" x)))

(defn delete_ref [working_directory db branch]
  (io/delete-file (io/file (str working_directory "/" db "/refs/heads/" branch)))
  (println (str "Deleted branch " branch ".")))

(defn print_branches [working_directory db]
  (let [refs (sort (seq (.list (io/file (str working_directory "/" db "/refs/heads")))))]
    (doseq [branch refs]
      (if (= 0 (compare (str  "ref: refs/heads/" branch "\n") (slurp (io/file (str working_directory "/" db "/HEAD")))))
        (println (str "* " branch))
        (println (str "  " branch))))))

(defn branch [working_directory db content]
  (let [instruction (first content)
        refs (keep-indexed #(cond (odd? %1) %2) content)
        switches (keep-indexed #(cond (even? %1) %2) content)
        additional_info (rest content)
        branch (first additional_info)]

    (cond (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-branch)
          (and (every? true? (map compare_d switches)) (> (count switches) (count refs))) (println "Error: you must specify a branch name.")
          (not (every? true? (map compare_d switches))) (println "Error: invalid arguments.")
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (and (= 0 (compare instruction "-d")) (not (.exists (io/file (str working_directory "/" db "/refs/heads/" branch))))) (println (str "Error: branch '" branch "' not found."))
          (and (= 0 (compare instruction "-d")) (= 0 (compare (str  "ref: refs/heads/" branch "\n") (slurp (io/file (str working_directory "/" db "/HEAD")))))) (println (str "Error: cannot delete checked-out branch '" branch "'."))
          (= 0 (compare instruction "-d")) (delete_ref working_directory db branch)
          :else (print_branches working_directory

                                db))))


