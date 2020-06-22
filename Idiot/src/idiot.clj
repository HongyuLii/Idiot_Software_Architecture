(ns idiot
  (:require [clojure.java.io :as io]
            help init compute_address_blob write_wtree commit_tree hash_object cat_file rev_parse switch branch commit rev_list log explore))

(defn create_dirs [dir datebase]
  (init/init dir datebase nil))

(defn d_switch [current_dir content]
  (cond (empty? content) (println "Error: the -d switch needs an argument")
        :else (let [db (first content)
                    function_and_content (rest content)
                    function (first function_and_content)
                    function_content (rest function_and_content)]
                (cond (not (.exists (io/file (str current_dir "/" db)))) (create_dirs current_dir db))
                (cond (= 0 (compare function "hash-object")) (hash_object/hash-object current_dir db function_content)
                      (= 0 (compare function "cat-file")) (cat_file/cat-file current_dir db function_content)
                      (= 0 (compare function "write-wtree")) (write_wtree/write_wtree current_dir db function_content)
                      (= 0 (compare function "commit-tree")) (commit_tree/commit-tree current_dir db function_content)
                      (= 0 (compare function "rev-parse")) (rev_parse/rev-parse current_dir db function_content)
                      (= 0 (compare function "switch")) (switch/switch current_dir db function_content)
                      (= 0 (compare function "commit")) (commit/commit current_dir db function_content)
                      (= 0 (compare function "rev-list")) (rev_list/rev-list current_dir db function_content)
                      (= 0 (compare function "log")) (log/log current_dir db function_content)
                      (= 0 (compare function "explore")) (explore/explore current_dir db function_content)
                      (= 0 (compare function "branch")) (branch/branch current_dir db function_content)))))

(defn r_switch [content]
  (let [db ".idiot"]
    (cond (empty? content) (println "Error: the -r switch needs an argument")
          :else (let [current_dir  (first content)
                      function_and_content (rest content)
                      function (first function_and_content)
                      function_content (rest function_and_content)]
                  (if (not (.exists (io/file current_dir)))
                    (println "Error: the directory specified by -r does not exist")
                    (cond (or (= nil function) (= 0 (compare function "--help")) (= 0 (compare function "-h"))) (help/top-level)
                          (= 0 (compare function "-d")) (d_switch current_dir function_content)
                          (= 0 (compare function "init")) (init/init current_dir db function_content)
                          (= 0 (compare function "hash-object")) (hash_object/hash-object current_dir db function_content)
                          (= 0 (compare function "cat-file")) (cat_file/cat-file current_dir db function_content)
                          (= 0 (compare function "write-wtree")) (write_wtree/write_wtree current_dir db function_content)
                          (= 0 (compare function "commit-tree")) (commit_tree/commit-tree current_dir db function_content)
                          (= 0 (compare function "rev-parse")) (rev_parse/rev-parse current_dir db function_content)
                          (= 0 (compare function "switch")) (switch/switch current_dir db function_content)
                          (= 0 (compare function "commit")) (commit/commit current_dir db function_content)
                          (= 0 (compare function "rev-list")) (rev_list/rev-list current_dir db function_content)
                          (= 0 (compare function "log")) (log/log current_dir db function_content)
                          (= 0 (compare function "explore")) (explore/explore current_dir db function_content)
                          (= 0 (compare function "branch")) (branch/branch current_dir db function_content)))))))

(defn -main [& args]
  (let
   [functions #{"help" "init" "hash-object" "cat-file" "-h" "--help" "write-wtree" "commit-tree" "-r" "-d" nil}
    function (first args)
    content (rest args)]
    (cond (not (contains? functions function)) (println "Error: invalid command")
          :else (cond (= 0 (compare function "-r")) (r_switch content)
                      (= 0 (compare function "-d")) (println "Error: the -d switch needs an argument")
                      (or (= nil function) (= 0 (compare function "--help")) (= 0 (compare function "-h"))) (help/top-level)
                      (= 0 (compare function "help")) (help/help content)
                      (= 0 (compare function "cat-file")) (cat_file/cat-file "." ".idiot" content)))))