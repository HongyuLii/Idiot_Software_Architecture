(ns explore
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [hiccup.page :refer [html5]]
            [clojure.java.io :as io]
            [ring.util.request :as request]
            commit
            log
            cat_file
            [clojure.string :as str])
  (:import (java.util.zip InflaterInputStream)
           (java.io ByteArrayOutputStream)))

(defn help-instructions-explore []
  (println "idiot explore: start a web server to explore the database\n")
  (println "Usage: idiot explore [-p <port>]\n")
  (println "Arguments:")
  (println "   -p <port>   listen on the given port (default: 3000)"))

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

(defn stringToNumber
  [& args]
  (apply + (map #(Integer/parseInt %)
                (filter #(re-matches #"\d+" %)
                        args))))

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
      (not (some #(= 0 (compare % later_address)) addresses_for_judge)) (str "nonexist")
      (some #(= 0 (compare % later_address)) repeated-seq) (str "repeated"))))

(defn return_addresses [first_two address all_shorted_addresses all_addresses selected_addresses]
  (let [index (.indexOf all_shorted_addresses address)]
    (println index)
    (cond (and (> (count all_shorted_addresses) 1) (not (= -1 index))) (return_addresses first_two address (subvec all_shorted_addresses (+ 1 index)) (subvec all_addresses (+ 1 index)) (conj selected_addresses (str first_two (nth all_addresses index))))
          :else (conj selected_addresses (str first_two (nth all_addresses index))))))

(defn get_full_addresses [working_directory db address]
  (let [first_two (subs address 0 2)
        later_address (subs address 2)
        all_addresses (seq (.list (io/file (str working_directory "/" db "/objects/" first_two))))
        later_address_length (count later_address)
        times (repeat (count all_addresses) later_address_length)
        addresses_for_judge (map adjust_length all_addresses times)]
    (return_addresses first_two later_address (vec addresses_for_judge) (vec all_addresses) [])))

(defn print-information [working_directory db address contentlist]
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
      (cond (= 0 (compare type "parent")) (print-information working_directory db parent-address (conj contentlist (str (subs address 0 7) " "  message)))
            :else (conj contentlist (str (subs address 0 7) " "  message))))))

(defn get_body_content [working_directory db uri]
  (let [ref (subs (slurp (io/file (str working_directory  "/" db "/HEAD"))) 16)
        list (sort (seq (.list (io/file (str working_directory "/" db "/refs/heads")))))
        contentlist ()]
    (cond (= 0 (compare uri "/")) (html5 [:head [:title "Test"]] [:body [:div {:class "head-info"} "HEAD points to ref " [:a {:href (clojure.string/trimr (str "/branch/" ref))} (clojure.string/trimr ref)]]
                                                                  [:ul {:class "branch-list"} (for [refs (vec list)] [:li [:a {:href (str "/branch/" refs)} refs]])]])

          (= 0 (compare (subs uri 0 3) "/br")) (let [list (reverse (log/print-addresses working_directory db (str/trim-newline (slurp (io/file (str working_directory "/" db "/refs/heads/" (subs uri 8))))) contentlist))]
                                                 (html5 [:head [:title "Test"]]
                                                        [:body [:ul {:class "commit-list"} (for [commit (vec list)] [:li [:a {:href (str "/commit/" (subs commit 0 7))} (subs commit 0 7)] (subs commit 7)])]]))

          (= 0 (compare (subs uri 0 3) "/co")) (let [address (cat_file/get_full_address working_directory db (subs uri 8))
                                                     body_info_string (cat_file/print-content working_directory db address)
                                                     index_of_author (.indexOf body_info_string "author")
                                                     index_of_double_newline (.indexOf body_info_string "\n\n")
                                                     info_without_author (subs body_info_string 0 index_of_author)
                                                     parent_vec (subvec (clojure.string/split-lines info_without_author) 1)
                                                     info_from_author (subs body_info_string index_of_author)
                                                     author_info (nth (clojure.string/split-lines info_from_author) 0)
                                                     commiter_info (nth (clojure.string/split-lines info_from_author) 1)
                                                     left_arrow (.indexOf author_info "<")
                                                     right_arrow (.indexOf author_info ">")
                                                     left_email (.indexOf commiter_info "<")
                                                     right_email (.indexOf commiter_info ">")]
                                                 (html5 [:head [:title "commit"]]
                                                        [:body [:h1 (str "Commit " (subs uri 8))]]
                                                        [:div {:class "tree"} "tree " [:a {:href (str "/tree/8430bcd2a813f5cc9171b68befafe08c7b6c5336")} "8430bcd2a813f5cc9171b68befafe08c7b6c5336"]]
                                                        (for [parent parent_vec] [:div {:class "parent"} "parent " [:a {:href (str "/commit/" (subs parent 7))} (subs parent 7)]])
                                                        [:div {:class "author"} (str (subs author_info 0 left_arrow)  "&lt;" (subs author_info (+ 1 left_arrow) right_arrow) "&gt;" (subs author_info (+ 1 right_arrow)))]
                                                        [:div {:class "committer"} (str (subs commiter_info 0 left_email) "&lt;" (subs commiter_info (+ 1 left_email) right_email) "&gt;"  (subs commiter_info (+ 1 right_email)))]
                                                        [:pre {:class "message"} (subs body_info_string (+ 2 index_of_double_newline))]))
          (= 0 (compare (subs uri 0 3) "/tr")) (let [address (cat_file/get_full_address working_directory db (subs uri 6))
                                                     entries (cat_file/print_tree working_directory db (second (cat_file/split-at-byte 0 (cat_file/unzip_path (cat_file/add_path working_directory db (cat_file/get_full_address working_directory db address))))) [])]
                                                 (html5 [:head [:title "tree"]]
                                                        [:body [:h1 (str "Tree " (subs uri 6))]
                                                         [:ul.tree-entries (for [entry entries] [:li [:tt (str (subs entry 0 12))
                                                                                                      [:a {:href (str "/" (subs entry 7 11) "/" (subs entry 12 52))} (subs entry 12 52)]
                                                                                                      (str " " (subs entry 53))]])]]))

          (= 0 (compare (subs uri 0 3) "/bl")) (let [address (cat_file/get_full_address working_directory db (subs uri 6))
                                                     body_info (cat_file/print-content working_directory db address)]
                                                 (html5 [:head [:title "blob"]]
                                                        [:body [:h1 (str "Blob " (subs uri 6))] [:pre body_info]])))))

(defn server_okay [working_directory db uri]
  (let [body (get_body_content working_directory db uri)] ;html5 [:head [:title "Test"]] [:body [:p {:style "color: green"} refs]]
    {:status 200  ; meaning "OK"
     :headers {"Content-Type" "text/html"}  ; instead of e.g. "text/html"
     :body body}))

(defn server_not []
  {:status 404  ; meaning "not found"
   :headers {"Content-Type" "text/html"}})

(defn server_multiple [working_directory db address]
  (let [all_addresses (get_full_addresses working_directory db address)
        body (html5 [:head [:title "Ambiguous"]]
                    [:body [:p "The given address prefix is ambiguous. Please disambiguate your intent by choosing from the following options."]
                     [:ul.disambiguation-list (for [full_address all_addresses] [:li [:a {:href (str "/" (cat_file/type_assesment working_directory db full_address) "/" full_address)} full_address] (str " (" (cat_file/type_assesment working_directory db full_address) ")")])]])]
    {:status 300  ; meaning "multiple"
     :headers {"Content-Type" "text/html"}
     :body body}))

(defn server_found [type address]
  {:status 302  ; meaning "found"
   :headers {"Location" (str "/" type "/" address)}
   :body ()})

(defn start_server [working_directory db port]
  (letfn [(handler [request]
            (let [uri (subs (request/request-url request) 21)]
              (cond (= 0 (compare uri "/")) (server_okay working_directory db uri)
                    (= 0 (compare (subs uri 0 3) "/br")) (cond (not (.exists (io/file (str working_directory "/" db "/refs/heads" (subs uri 7))))) (server_not)
                                                               :else (server_okay working_directory db uri))
                    (= 0 (compare (subs uri 0 3) "/co")) (cond (= 0 (compare "nonexist" (if-repeated working_directory db (subs uri 8)))) (server_not)
                                                               (= 0 (compare "repeated" (if-repeated working_directory db (subs uri 8)))) (server_multiple working_directory db (subs uri 8))
                                                               (= 0 (compare "tree" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 8))))) (server_found "tree" (subs uri 8))
                                                               (= 0 (compare "blob" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 8))))) (server_found "blob" (subs uri 8))
                                                               :else (server_okay working_directory db uri))
                    (= 0 (compare (subs uri 0 3) "/tr")) (cond (= 0 (compare "nonexist" (if-repeated working_directory db (subs uri 6)))) (server_not)
                                                               (= 0 (compare "repeated" (if-repeated working_directory db (subs uri 6)))) (server_multiple working_directory db (subs uri 6))
                                                               (= 0 (compare "commit" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 6))))) (server_found "commit" (subs uri 6))
                                                               (= 0 (compare "blob" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 6))))) (server_found "blob" (subs uri 6))
                                                               :else (server_okay working_directory db uri))
                    (= 0 (compare (subs uri 0 3) "/bl")) (cond (= 0 (compare "nonexist" (if-repeated working_directory db (subs uri 6)))) (server_not)
                                                               (= 0 (compare "repeated" (if-repeated working_directory db (subs uri 8)))) (server_multiple working_directory db (subs uri 6))
                                                               (= 0 (compare "commit" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 6))))) (server_found "commit" (subs uri 6))
                                                               (= 0 (compare "tree" (cat_file/type_assesment working_directory db (commit/get_full_address working_directory db (subs uri 6))))) (server_found "tree" (subs uri 6))
                                                               :else (server_okay working_directory db uri)))))
          (start-server [port]
            (println (str "Starting server on port " port "."))
            (run-jetty handler {:port port}))]
    (start-server port)))

(defn explore [working_directory db content]
  (let [p (first content)
        port (second content)]
    (cond (or (= 0 (compare p "--help")) (= 0 (compare p "-h"))) (help-instructions-explore)
          (not (.exists (io/file (str working_directory "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (and (= 0 (compare p "-p")) (empty? port)) (println "Error: you must specify a numeric port with '-p'.")
          (and (= 0 (compare p "-p")) (<= (stringToNumber port) 0)) (println "Error: the argument for '-p' must be a non-negative integer.")
          :else (cond (empty? port) (start_server working_directory db 3000)
                      :else (start_server working_directory db (stringToNumber port))))))
