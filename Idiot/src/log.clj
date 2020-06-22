(ns log
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.zip InflaterInputStream)
           (java.io ByteArrayOutputStream)))

(defn help-instructions-log []
  (println "idiot log: print abbreviated commit addresses and commit summaries\n")
  (println "Usage: idiot log --oneline [-n <count>] [<ref>]\n")
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

(defn print-count-addresses [working_directory db count address contentlist]
  (let [count_current (- count 1)]
    (with-open [input (-> (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)) io/file io/input-stream)]
      (let [full-content (unzip input)
            startpoint (.indexOf full-content  "\000")
            full-commit (subs full-content (+ 12 startpoint))
            startpoint-getridoftree (.indexOf full-commit "\n")
            content-without-tree (subs full-commit (+ 1 startpoint-getridoftree))
            type (subs content-without-tree 0 6)
            parent-address (subs content-without-tree 7 47)
            committer-point (.lastIndexOf full-commit "committer")
            from-committer (subs full-commit committer-point)
            committer-newline-point (.indexOf from-committer "\n")
            after-newline-from-last-blank-content (subs from-committer (+ 1 committer-newline-point))
            last-newline-point-before-message (.indexOf after-newline-from-last-blank-content "\n")
            full-message (subs after-newline-from-last-blank-content (+ 1 last-newline-point-before-message))
            first-newline-in-massage (.indexOf full-message "\n")
            message (subs full-message 0 first-newline-in-massage)]
        (println (str (subs address 0 7) " " message))
        (cond (and (= 0 (compare type "parent")) (not (= count_current 0))) (print-count-addresses working_directory db count_current parent-address (conj contentlist (str (subs address 0 7) " "  message)))
              :else (conj contentlist (str (subs address 0 7) " " message)))))))

(defn print-addresses [working_directory db address contentlist]
  (with-open [input (-> (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)) io/file io/input-stream)]
    (let [full-content (unzip input)
          startpoint (.indexOf full-content  "\000")
          full-commit (subs full-content (+ 12 startpoint))
          startpoint-getridoftree (.indexOf full-commit "\n")
          content-without-tree (subs full-commit (+ 1 startpoint-getridoftree))
          type (subs content-without-tree 0 6)
          parent-address (subs content-without-tree 7 47)
          committer-point (.lastIndexOf full-commit "committer")
          from-committer (subs full-commit committer-point)
          committer-newline-point (.indexOf from-committer "\n")
          after-newline-from-last-blank-content (subs from-committer (+ 1 committer-newline-point))
          last-newline-point-before-message (.indexOf after-newline-from-last-blank-content "\n")
          full-message (subs after-newline-from-last-blank-content (+ 1 last-newline-point-before-message))
          first-newline-in-massage (.indexOf full-message "\n")
          message (subs full-message 0 first-newline-in-massage)]
      (println (str (subs address 0 7) " "  message))
      (cond (= 0 (compare type "parent")) (print-addresses working_directory db parent-address (conj contentlist (str (subs address 0 7) " "  message)))
            :else (conj contentlist (str (subs address 0 7) " "  message))))))

(defn stringToNumber
  [& args]
  (apply + (map #(Integer/parseInt %)
                (filter #(re-matches #"\d+" %)
                        args))))

(defn log [working_directory db content]
  (let [online (first content)
        left (rest content)
        n (first left)
        left_two (rest left)
        count (first left_two)
        ref (second left_two)
        contentlist ()]
    (cond (or (= 0 (compare online "--help")) (= 0 (compare online "-h"))) (help-instructions-log)
          (not (= 0 (compare online "--oneline"))) (println "Error: log requires the --oneline switch")
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (and (= 0 (compare n "-n")) (empty? left_two)) (println "Error: you must specify a numeric count with '-n'.")
          (and (= 0 (compare n "-n")) (<= (stringToNumber count) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
          (empty? left) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))) contentlist)
                              :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD")))) contentlist))
          (= 0 (compare n "-n")) (cond (or (= 0 (compare ref "@")) (= 0 (compare ref "HEAD")) (empty? ref)) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))) contentlist)
                                                                                                                  :else (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD")))) contentlist))
                                       (not (.exists (io/file (str working_directory "/" db "/refs/heads/" ref)))) (println (str "Error: could not find ref named " ref "."))
                                       :else (print-count-addresses working_directory db (stringToNumber count) (str/trim-newline (slurp (io/file (str working_directory "/" db "/refs/heads/" ref)))) contentlist))
          (not (= 0 (compare n "-n"))) (cond (or (= 0 (compare n "@")) (= 0 (compare n "HEAD"))) (cond (= 0 (compare "ref:" (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 0 4))) (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/" (str/trim-newline (subs (slurp (io/file (str working_directory "/" db "/HEAD"))) 5)))))) contentlist)
                                                                                                       :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/HEAD")))) contentlist))
                                             (not (.exists (io/file (str working_directory "/" db "/refs/heads/" n)))) (println (str "Error: could not find ref named " n "."))
                                             :else (print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/refs/heads/" n)))) contentlist)))))
