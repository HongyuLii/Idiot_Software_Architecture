(ns rev_list
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.zip InflaterInputStream)
           (java.io ByteArrayOutputStream)))

(defn help-instructions-rev-list []
  (println "idiot rev-list: list preceding revisions, latest first\n")
  (println "Usage: idiot rev-list [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

(defn print-count-addresses [working_directory db count address]
  (let [count_current (- count 1)]
    (with-open [input (-> (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)) io/file io/input-stream)]
      (let [full-content (unzip input)
            startpoint (.indexOf full-content  "\000")
            full-commit (subs full-content (+ 12 startpoint))
            startpoint-getridoftree (.indexOf full-commit "\n")
            content-without-tree (subs full-commit (+ 1 startpoint-getridoftree))
            type (subs content-without-tree 0 6)
            parent-address (subs content-without-tree 7 47)]
        (println (str address))
        (cond (and (= 0 (compare type "parent")) (not (= count_current 0))) (print-count-addresses working_directory db count_current parent-address))))))

(defn print-addresses [working_directory db address]
  (with-open [input (-> (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)) io/file io/input-stream)]
    (let [full-content (unzip input)
          startpoint (.indexOf full-content  "\000")
          full-commit (subs full-content (+ 12 startpoint))
          startpoint-getridoftree (.indexOf full-commit "\n")
          content-without-tree (subs full-commit (+ 1 startpoint-getridoftree))
          type (subs content-without-tree 0 6)
          parent-address (subs content-without-tree 7 47)]
      (println (str address))
      (cond (= 0 (compare type "parent")) (print-addresses working_directory db parent-address)))))

(defn stringToNumber
  [& args]
  (apply + (map #(Integer/parseInt %)
                (filter #(re-matches #"\d+" %)
                        args))))

(defn rev-list [working_directory db content]
  (let [n (first content)
        left (rest content)
        count (first left)
        ref (second left)]

    (cond (or (= 0 (compare n "--help")) (= 0 (compare n "-h"))) (help-instructions-rev-list)
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (and (= 0 (compare n "-n")) (empty? left)) (println "Error: you must specify a numeric count with '-n'.")
          (and (= 0 (compare n "-n")) (<= (stringToNumber count) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
          (empty? content) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))))
                                 :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD"))))))
          (= 0 (compare n "-n")) (cond (or (= 0 (compare ref "@")) (= 0 (compare ref "HEAD")) (empty? ref)) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))))
                                                                                                                  :else (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD"))))))
                                       (not (.exists (io/file (str working_directory "/" db "/refs/heads/" ref)))) (println (str "Error: could not find ref named " ref "."))
                                       :else (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/refs/heads/" ref))))))
          (not (= 0 (compare n "-n"))) (cond (or (= 0 (compare n "@")) (= 0 (compare n "HEAD"))) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))))
                                                                                                       :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD"))))))
                                             (not (.exists (io/file (str working_directory "/" db "/refs/heads/" n)))) (println (str "Error: could not find ref named " n "."))
                                             :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/refs/heads/"

                                                                                                                                n)))))))))
