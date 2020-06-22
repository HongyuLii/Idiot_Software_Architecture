(ns commit_tree
  (:import java.security.MessageDigest
           java.util.zip.InflaterInputStream
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream))
  (:require [clojure.java.io :as io]
            help compute_address_blob))

(defn help-instructions-commit-tree []
  (println "idiot commit-tree: write a commit object based on the given tree\n")
  (println "Usage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n")
  (println "Arguments:")
  (println "   -h               print this message")
  (println "   <tree>           the address of the tree object to commit")
  (println "   -m \"<message>\"   the commit message")
  (println "   -p <parent>      the address of a parent commit"))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
              content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

(defn add_path [working_directory db tree_address]
  (let [db db
        dir (str working_directory "/" db "/objects/" (subs tree_address 0 2))]
    (apply str dir "/" (subs tree_address 2))))

(defn get-path [working_directory db address]
  (let [db db
        dir (str working_directory "/" db "/objects/" (subs address 0 2))
        full-path (str dir "/" (subs address 2))]
    (.mkdir (io/file dir))
    (.createNewFile (io/file full-path))
    (apply str dir "/" (subs address 2))))

(defn repeated [coll]
  (->> coll
       frequencies
       (remove #(= 1 (val %)))
       keys))

(defn adjust_length [string length]
  (subs string 0 length))

(defn if-repeated [working_directory db address]
  (let [first_two (subs address 0 2)
        later_address (subs address 2)
        all_addresses (seq (.list (io/file (str working_directory "/" db "/objects/" first_two))))
        later_address_length (count later_address)
        times (repeat (count all_addresses) later_address_length)
        addresses_for_judge (map adjust_length all_addresses times)
        repeated-seq (repeated addresses_for_judge)]
    (cond
      (not (some #(= 0 (compare % later_address)) addresses_for_judge)) (str "noneexist")
      (some #(= 0 (compare % later_address)) repeated-seq) (str "repeated"))))

(defn get_full_address [working_directory db address]
  (let [first_two (subs address 0 2)
        later_address (subs address 2)
        all_addresses (seq (.list (io/file (str working_directory "/" db "/objects/" first_two))))
        later_address_length (count later_address)
        times (repeat (count all_addresses) later_address_length)
        addresses_for_judge (map adjust_length all_addresses times)
        target-index (.indexOf (vec addresses_for_judge) later_address)]
    (str first_two (nth all_addresses target-index))))

(defn build_commit_object_0 [working_directory db tree_address message]
  (let [commit-object (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
                            tree-str tree_address
                            message message
                            commit-format (str "tree %s\n"
                                               "author %s\n"
                                               "committer %s\n"
                                               "\n"
                                               "%s\n")
                            commit-str (format commit-format
                                               tree-str
                                               author-str
                                               author-str
                                               message)]
                        (format "commit %d\000%s"
                                (count commit-str)
                                commit-str))

        commit-addr (sha-bytes (.getBytes commit-object))

        hex_address (to-hex-string commit-addr)]
    (io/copy (zip-str  (str "commit " (count commit-object)  "\000" commit-object)) (io/file (get-path working_directory db hex_address)))
    (to-hex-string commit-addr)))

(defn build_commit_object_1 [working_directory db tree_address message parent_info]
  (let [commit-object (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
                            tree-str tree_address
                            message message
                            parent-str (nth parent_info 1)
                            parent-str-address (str (subs parent-str 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs parent-str 0 2)))))))
                            commit-format (str "tree %s\n"
                                               "parent %s\n"
                                               "author %s\n"
                                               "committer %s\n"
                                               "\n"
                                               "%s\n")
                            commit-str (format commit-format
                                               tree-str
                                               parent-str-address
                                               author-str
                                               author-str
                                               message)]
                        (format "commit %d\000%s"
                                (count commit-str)
                                commit-str))

        commit-addr (sha-bytes (.getBytes commit-object))
        hex_address (to-hex-string commit-addr)]
    (io/copy (zip-str  (str "commit " (count commit-object)  "\000" commit-object)) (io/file (get-path working_directory db hex_address)))
    (to-hex-string commit-addr)))

(defn build_commit_object_2 [working_directory db tree_address message parent_info]
  (let [commit-object (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
                            tree-str tree_address
                            first_parent (nth parent_info 1)
                            first_parent_address (str (subs first_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs first_parent 0 2)))))))
                            second_parent (nth parent_info 3)
                            second_parent_address (str (subs second_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs second_parent 0 2)))))))
                            message message
                            commit-format (str "tree %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "author %s\n"
                                               "committer %s\n"
                                               "\n"
                                               "%s\n")
                            commit-str (format commit-format
                                               tree-str
                                               first_parent_address
                                               second_parent_address
                                               author-str
                                               author-str
                                               message)]
                        (format "commit %d\000%s"
                                (count commit-str)
                                commit-str))

        commit-addr (sha-bytes (.getBytes commit-object))

        hex_address (to-hex-string commit-addr)]
    (io/copy (zip-str  (str "commit " (count commit-object)  "\000" commit-object)) (io/file (get-path working_directory db hex_address)))
    (to-hex-string commit-addr)))

(defn build_commit_object_8 [working_directory db tree_address message parent_info]
  (let [commit-object (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
                            tree-str tree_address
                            first_parent (nth parent_info 1)
                            first_parent_address (str (subs first_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs first_parent 0 2)))))))
                            second_parent (nth parent_info 3)
                            second_parent_address (str (subs second_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs second_parent 0 2)))))))
                            third_parent (nth parent_info 5)
                            third_parent_address (str (subs third_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs third_parent 0 2)))))))
                            fourth_parent (nth parent_info 7)
                            fourth_parent_address (str (subs fourth_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs fourth_parent 0 2)))))))
                            fifth_parent (nth parent_info 9)
                            fifth_parent_address (str (subs fifth_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs fifth_parent 0 2)))))))
                            sixth_parent (nth parent_info 11)
                            sixth_parent_address (str (subs sixth_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs sixth_parent 0 2)))))))
                            seventh_parent (nth parent_info 13)
                            seventh_parent_address (str (subs seventh_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs seventh_parent 0 2)))))))
                            eighth_parent (nth parent_info 15)
                            eighth_parent_address (str (subs eighth_parent 0 2) (first (seq (.list (io/file (str working_directory "/" db "/objects/" (subs eighth_parent 0 2)))))))
                            message message
                            commit-format (str "tree %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "parent %s\n"
                                               "author %s\n"
                                               "committer %s\n"
                                               "\n"
                                               "%s\n")
                            commit-str (format commit-format
                                               tree-str
                                               first_parent_address
                                               second_parent_address
                                               third_parent_address
                                               fourth_parent_address
                                               fifth_parent_address
                                               sixth_parent_address
                                               seventh_parent_address
                                               eighth_parent_address
                                               author-str
                                               author-str
                                               message)]
                        (format "commit %d\000%s"
                                (count commit-str)
                                commit-str))

        commit-addr (sha-bytes (.getBytes commit-object))

        hex_address (to-hex-string commit-addr)]
    (io/copy (zip-str  (str "commit " (count commit-object)  "\000" commit-object)) (io/file (get-path working_directory db hex_address)))
    (to-hex-string commit-addr)))

(defn process_parent_info [working_directory db tree_address message parent_info]

  (if (empty? parent_info)
    (println (build_commit_object_0 working_directory db tree_address message))
    (let [p (first parent_info)
          parent_address (rest parent_info)
          first_address (first parent_address)
          length (count parent_info)]

      (cond (and (= 0 (compare p "-p")) (empty? parent_address)) (println "Error: you must specify a commit object with the -p switch.")
            (< (count first_address) 4) (println (str "Error: too few characters specified for address '" first_address "'"))
            (= 0 (compare "repeated" (if-repeated working_directory db first_address))) (println (str "Error: ambiguous match for address '" first_address "'"))
            (= 0 (compare "noneexist" (if-repeated working_directory db first_address))) (println (str "Error: no commit object exists at address " first_address "."))
            (not (.exists (io/file (add_path working_directory db (get_full_address working_directory db first_address))))) (println (str "Error: no commit object exists at address " first_address "."))
            (not (= 0 (compare (vec (byte-array (.getBytes "commit"))) (subvec (vec (unzip (add_path working_directory db (get_full_address working_directory db first_address)))) 0 6)))) (println (str "Error: an object exists at address " first_address ", but it isn't a commit."))
            :else (cond (= length 2) (println (build_commit_object_1 working_directory db tree_address message parent_info))
                        (= length 4) (println (build_commit_object_2 working_directory db tree_address message parent_info))
                        (= length 16) (println (build_commit_object_8 working_directory db tree_address message parent_info)))))))

(defn commit-tree [working_directory db content]
  (let [db db
        tree_address (first content)
        left (rest content)
        message_instruction (first left)
        message (second left)
        content_after_m (rest left)
        parent_info (rest content_after_m)]

    (cond (or (= 0 (compare tree_address "--help")) (= 0 (compare tree_address "-h"))) (help-instructions-commit-tree)
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (empty? tree_address) (println "Error: you must specify a tree address.")
          (< (count tree_address) 4) (println (str "Error: too few characters specified for address '" tree_address "'"))
          (= 0 (compare "repeated" (if-repeated working_directory db tree_address))) (println (str "Error: ambiguous match for address '" tree_address "'"))
          (= 0 (compare "noneexist" (if-repeated working_directory db tree_address))) (println "Error: no tree object exists at that address.")
          (not (= 0 (compare (vec (byte-array (.getBytes "tree"))) (subvec (vec (unzip (add_path working_directory db (get_full_address working_directory db tree_address)))) 0 4)))) (println "Error: an object exists at that address, but it isn't a tree.")
          (empty? message_instruction) (println "Error: you must specify a message.")        ;if no -m is given
          (empty? message) (println "Error: you must specify a message with the -m switch.") ;if no message is given
          :else (process_parent_info working_directory db (get_full_address working_directory db tree_address) message parent_info))))