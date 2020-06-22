(ns hash_object
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream))
  (:require [clojure.java.io :as io]
            help
            compute_address_blob
            compute_address_tree))

(defn help-instructions-hash []
  (println "idiot hash-object: compute address and maybe create blob from file\n")
  (println "Usage: idiot hash-object [-w] <file>\n")
  (println "Arguments:")
  (println "   -h       print this message")
  (println "   -w       write the file to database as a blob object")
  (println "   <file>   the file"))

(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
              content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn get-path [working_directory db address]
  (let
   [dir (str working_directory "/" db "/objects/" (subs address 0 2))
    full-path (str dir "/" (subs address 2))]
    (.mkdir (io/file dir))
    (.createNewFile (io/file full-path))
    (apply str dir "/" (subs address 2))))

(defn save-file [working_directory db file]
  (cond (empty? file) (println "Error: you must specify a file.")
        (not (.exists (io/file (str working_directory "/" file)))) (println "Error: that file isn't readable")
        :else (let [binary_address (sha-bytes (.getBytes (str "blob " (count (slurp (str working_directory "/" file))) "\000" (slurp (str working_directory "/" file)))))
                    hex_address (to-hex-string binary_address)]
                (io/copy (zip-str (str "blob " (count (slurp (str working_directory "/" file))) "\000" (slurp (str working_directory "/" file)))) (io/file (get-path working_directory db hex_address)))
                (println hex_address))))

(defn hash-object [working_directory db content]
  (let [db db
        instruction (first content)
        left (rest content)
        file (first left)]
    (cond (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-hash)
          (not (.exists (io/file working_directory))) (println "Could not find directory")
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (= 0 (compare instruction "-w")) (save-file working_directory db file)
          (= instruction nil) (println "Error: you must specify a file.")
          (not (.exists (io/file (str working_directory "/" instruction)))) (println "Error: that file isn't readable")
          (.exists (io/file (str working_directory "/" instruction))) (println (to-hex-string (sha-bytes (.getBytes (str "blob " (count (slurp (str working_directory "/" instruction))) "\000" (slurp (str working_directory "/"

                                                                                                                                                                                                            instruction))))))))))