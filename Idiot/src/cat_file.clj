(ns cat_file
  (:import
   java.util.zip.InflaterInputStream
   (java.io ByteArrayOutputStream))
  (:require [clojure.java.io :as io]
            help
            compute_address_blob
            compute_address_tree))

(defn help-instructions-cat []
  (println "idiot cat-file: print information about an object\n")
  (println "Usage: idiot cat-file {-p|-t} <address>\n")
  (println "Arguments:")
  (println "   -h          print this message")
  (println "   -p          pretty-print contents based on object type")
  (println "   -t          print the type of the given object")
  (println "   <address>   the SHA1-based address of the object"))

(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

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

(defn unzip_path
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

(defn add_path [working_directory db tree_address]
  (let [dir (str working_directory "/" db "/objects/" (subs tree_address 0 2))]
    (apply str dir "/" (subs tree_address 2))))

(defn print_tree [working_directory db tree_content tree_list]
  (let [first_info (bytes->str (first (split-at-byte 0 tree_content)))
        divide_point (.indexOf first_info " ")
        first_mode (subs first_info 0 divide_point)
        first_path (subs first_info (+ 1 divide_point))
        address (to-hex-string (take 20 (second (split-at-byte 0 tree_content))))
        next_content (subvec (vec (second (split-at-byte 0 tree_content))) 20)]

    (cond (empty? next_content) (cond (= 0 (compare "100644" first_mode)) (conj tree_list (str "100644 blob " address "\t" first_path))
                                      (= 0 (compare "40000" first_mode))  (conj tree_list (str "040000 tree " address "\t" first_path)))
          :else (cond (= 0 (compare "100644" first_mode)) (print_tree working_directory db next_content (conj tree_list (str "100644 blob " address "\t" first_path)))
                      (= 0 (compare "40000" first_mode))  (print_tree working_directory db next_content (conj tree_list (str "040000 tree " address "\t" first_path)))))))

(defn print-content [working_directory db address]
  (let [db db]
    (cond (or (not (.exists (io/file (str working_directory "/" db "/objects/" (subs address 0 2))))) (not (.exists (io/file (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)))))) (println "Error: that address doesn't exist")
          :else (with-open [input (-> (str working_directory "/" db "/objects/" (subs address 0 2) "/" (subs address 2)) io/file io/input-stream)]
                  (let [full-content (unzip input)
                        startpoint (.indexOf full-content  "\000")]
                    (cond (= 0 (compare "commit" (subs full-content 0 6))) (subs full-content (+ 12 startpoint))
                          (= 0 (compare "blob" (subs full-content 0 4))) (subs full-content (+ 1 startpoint))))))))

(defn type_assesment [working_directory db address]
  (cond (= 0 (compare (vec (byte-array (.getBytes "tree"))) (subvec (vec (unzip_path (add_path working_directory db address))) 0 4))) (str "tree")
        (= 0 (compare (vec (byte-array (.getBytes "blob"))) (subvec (vec (unzip_path (add_path working_directory db address))) 0 4))) (str "blob")
        (= 0 (compare (vec (byte-array (.getBytes "commit"))) (subvec (vec (unzip_path (add_path working_directory db address))) 0 6))) (str "commit")))

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

(defn cat-file [working_directory db content]
  (let [instruction (first content)
        address (second content)]
    (cond (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-cat)
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (or (empty? instruction) (not (or (= 0 (compare "-p" instruction)) (= 0 (compare "-t" instruction))))) (println "Error: the -p or -t switch is required")
          (or (> (count address) 40) (empty? address)) (println "Error: you must specify an address")
          (< (count address) 4) (println (str "Error: too few characters specified for address '" address "'"))
          (= 0 (compare "repeated" (if-repeated working_directory db address))) (println (str "Error: ambiguous match for address '" address "'"))
          (= 0 (compare "noneexist" (if-repeated working_directory db address))) (println "Error: that address doesn't exist")
          (= 0 (compare instruction "-t")) (println (type_assesment working_directory db (get_full_address working_directory db address)))
          (= 0 (compare instruction "-p")) (cond (= 0 (compare "tree" (type_assesment working_directory db (get_full_address working_directory db address)))) (doseq [entry (print_tree working_directory db (second (split-at-byte 0 (unzip_path (add_path working_directory db (get_full_address working_directory db address))))) [])]
                                                                                                                                                                (println entry))
                                                 :else (print (print-content working_directory db (get_full_address working_directory db address)))))))
