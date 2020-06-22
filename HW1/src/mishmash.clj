(ns mishmash)

(defn pascal [x]
  (dotimes [i x]
    (def a i)
    (def b x)
    (def res 1)
    (cond (> a (- b a))
          (def a (- b a)))
    (dotimes [j a]
      (def res (* res (- b j)))
      (def res (/ res (+ 1 j)))
      )
    (print res)
    (print " ")
    )
  (println 1)
  )


(defn write-roman [x]

  (def test x)

  (def numbers [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1])
  (def romans ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"])
  (def position "")
  (dotimes [i, 13]
    (while (>= test (nth numbers i))
      (def position (clojure.string/join  [position  (nth romans i)]))
      (def test (- test (nth numbers i)))
      )
    )
  (println position)
  )


(defn read-roman [x]
  (def input x)
  (def roman_num {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
  (def total 0)
  (def length_input (count input))
  (dotimes [i length_input]
    (def c (get input i))
    (def cn (get input (+ i 1)))
    (cond (and (= c \I) (or (= cn \V) (= cn \X))) (def total (- total 1))
          (and (= c \X) (or (= cn \L) (= cn \C))) (def total (- total 10))
          (and (= c \C) (or (= cn \D) (= cn \M))) (def total (- total 100))
          :else (def total (+ total (get roman_num c)))
          )
    )
  ;(def total (+ total (get roman_num (get "MCMXCIV" (- length_input 1)))))
  (println total)

  )


(defn judge_read [content]

  (def value (first content))
  (def roman_num #{\I, \V, \X, \L, \C, \D, \M})
  (def roman_chart {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
  (def roman_list {\I 1, \V 2, \X 3, \L 4, \C 5, \D 6, \M 7})

  (def legal true)
  (def length (count value))




  (dotimes [i length]
    (def current_value (get value i))
    (cond (not (contains? roman_num current_value)) (def legal false))

    )

  (def repeat_time 1)
  (def less_than_thice 1)


  (cond (true? legal) (
                        dotimes [i (- length 1)]
                        (def current_value (get value i))
                        (def next_value (get value (+ i 1)))

                        (cond (and (= current_value next_value) (or (= current_value \V) (= current_value \L) (= current_value \D))) (def legal false))
                        (cond (and (= current_value \V) (= next_value \X)) (def legal false))
                        (cond (and (= current_value \I) (or (= next_value \L) (= next_value \M) (= next_value \C) (= next_value \D))) (def legal false))


                        (cond (= current_value next_value) (def repeat_time (+ repeat_time 1)))
                        (cond (>= repeat_time 4) (def legal false))
                        (cond (not (= current_value next_value)) (def repeat_time 0))

                        (cond (< (get roman_chart current_value) (get roman_chart next_value)) (def less_than_thice (+ less_than_thice 1)))
                        (cond (>= less_than_thice 3) (def legal false))
                        (cond (> (get roman_chart current_value) (get roman_chart next_value)) (def less_than_thice 0))

                        (def index_current (get roman_list current_value))
                        (def index_next (get roman_list next_value))
                        (def diff (- index_next index_current))

                        (cond (and (< index_current index_next) (> diff 2) (not (= current_value \I))) (def legal false))


                        ))





  (cond (true? legal) (read-roman value)
        :else (println "invalid input"))

  )




(defn judge [function, content]

  (cond (= 0 (compare function "read-roman")) (judge_read content))

  (def input (map #(Integer/parseInt %)
                  (filter #(re-matches #"\d+" %)
                          content))
    )

  (def value (first input))

  (cond (not= 0 (compare function "read-roman"))
        (cond (not (integer? value)) (println "invalid input")
              (< value 0) (println "invalid input")
              :else (
                      cond (= 0 (compare function "pascal")) (pascal value)
                           (= 0 (compare function "write-roman")) (
                                                                    cond (<= value 0) (println "invalid input")
                                                                         (>= value 4000) (println "invalid input")
                                                                         :else (write-roman value)
                                                                         )
                           :else (
                                  (not= 0 (compare function "read-roman")) (println "invalid input")
                                  )

                           )
              )
        )

  )



(defn -main [& args]

  (def function (first args))

  (def content (rest args))
  (def num_of_function (count function))
  (def num_of_args (count content))

  (def ava false)

    (cond
          (< num_of_function 1) (def ava false)
          (> num_of_args 1) (def ava false)
          (< num_of_args 1) (def ava false)
          :else (def ava true))
  (cond (not (true? ava)) (println "invalid input")
        :else (judge function content))

  )








