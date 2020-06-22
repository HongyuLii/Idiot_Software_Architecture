(ns write_wtree
  (:require [clojure.java.io :as io]
            help
            compute_address_blob
            compute_address_tree))

(def entry_hash {})
(declare current_dir)
(declare write-address)

(defn help-instructions-write-tree []
  (println "idiot write-wtree: write the working tree to the database\n")
  (println "Usage: idiot write-wtree\n")
  (println "Arguments:")
  (println "   -h       print this message"))

(defn calculate_content [current_dir working_directory db file]
  (cond (empty? file) ()
        (and (not (= 0 (compare file db))) (.isDirectory (io/file (str working_directory "/" file)))) (write-address current_dir (str working_directory "/" file) file db)
        (and (not (= 0 (compare file db))) (not (.isDirectory (io/file (str working_directory "/" file))))) (concat (.getBytes (str "100644 " file "\000")) (compute_address_blob/compute_address current_dir db (str working_directory "/" file)))))

(defn write-address [current_dir working_directory currentFile db]
  (let [db db
        currentDirectory working_directory
        file_names (sort (seq (.list (io/file currentDirectory))))
        multi_directories (repeat (count file_names) currentDirectory)
        multi_db (repeat (count file_names) db)
        multi_cd (repeat (count file_names) current_dir)
        content_list (map calculate_content multi_cd multi_directories multi_db file_names)
        content_under_cd (reduce concat content_list)]
    (cond (empty? content_under_cd) ()
          (= 0 (compare currentDirectory current_dir)) (compute_address_tree/compute_address_hex current_dir db content_under_cd)
          :else (concat (.getBytes (str "40000 " currentFile "\000")) (compute_address_tree/compute_address current_dir db content_under_cd)))))

(defn write_wtree [current_dir db content]
  (let [db db
        instruction (first content)
        file_names (sort (seq (.list (io/file current_dir))))]
    (cond (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-write-tree)
          (not (.exists (io/file (str current_dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (seq instruction) (println "Error: write-wtree accepts no arguments")
          (or (= 0 (count file_names)) (and (= 0 (compare db (first file_names))) (= 1 (count file_names)))) (println "The directory was empty, so nothing was saved.")
          :else (println (write-address current_dir current_dir current_dir db)))))

