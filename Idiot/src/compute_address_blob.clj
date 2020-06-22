(ns compute_address_blob
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream)) (:require [clojure.java.io :as io]))

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

(defn compute_address [working_directory db file_name]

  (let [binary_address (sha-bytes (.getBytes (str "blob " (count (slurp file_name)) "\000" (slurp file_name))))
        hex_address (to-hex-string binary_address)
        dir (str working_directory "/" db "/objects/" (subs hex_address 0 2))
        full-path (str dir "/" (subs hex_address 2))]
    (cond (not (.exists (io/file full-path)))
          (io/copy (zip-str (str "blob " (count (slurp (io/file file_name))) "\000" (slurp (io/file file_name)))) (io/file (get-path working_directory db hex_address)))))

  (cond (.isFile (io/file file_name))
        (sha-bytes (.getBytes (str "blob " (count (slurp file_name)) "\000" (slurp

                                                                             file_name))))))




