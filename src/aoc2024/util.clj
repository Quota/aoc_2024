(ns aoc2024.util
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [clj-http.client :as client])
  (:import [java.io IOException]))

;;; parsing numbers

(defn parse-numbers
  "Parses a string of longs (or a list of strings of numbers, i.e. you
  can omit the `map` call).
  Input: \"n n ...\" or (\"n n ...\" \"...\") ; string (or seq of) of longs
  Output: (n n ...) or ((n ...) ...) ; list (or list of lists) of longs
  Argument `sep` is the regex to use to split numbers within strings,
  default is spaces (one or more).
  Argument `opts` is vector with options:
  - :vec -> produce vectors instead of lists"
  ([in]
   (parse-numbers #" +" in))
  ([what in]
   (if (vector? what)
     (parse-numbers #" " what in)
     (parse-numbers what [:std] in)))
  ([sep opts in]
   (if (coll? in)
     (let [map-fn (if (some #{:vec} opts) mapv map)]
       (->> in
            (map #(str/split % sep))
            (map-fn (fn[x] (map-fn #(parse-long %) x)))))
     (first (parse-numbers sep opts (hash-set in))))))

(comment
  (parse-numbers "1 2 3")
  (parse-numbers #"," "1,2,3")
  (parse-numbers [:vec] "1 2 3")
  (parse-numbers #":" [:vec] "1:2:3")
  (parse-numbers ["1 2 3" "4 5 6"])
  (parse-numbers #"," ["1,2,3" "4,5,6"])
  (parse-numbers [:vec] ["1 2 3" "4 5 6"])
  (parse-numbers #":" [:vec] (list "1:2:3" "4:5:6"))
  )

;;; getting the input

(def *session-cookie-global*
  (str (System/getProperty "user.home") "/.aoc-session-cookie.txt"))

(def *session-cookie-local*
  "var/session-cookie.txt")

(defn get-url
  [day]
  (str "https://adventofcode.com/2024/day/" day "/input"))

(defn get-session-cookie-value
  []
  (cond
    (.exists (io/file *session-cookie-global*)) (slurp *session-cookie-global*)
    (.exists (io/file *session-cookie-local*)) (slurp *session-cookie-local*)))

(defn get-input
  [day]
  (io/make-parents "var/dummy") ; ignore return of make-parents
  (let [filename (str "var/in-" day ".txt")]
    (when-not (-> filename io/file .exists)
      (if-let [cookie (get-session-cookie-value)]
        (spit filename
              (try
                (:body
                  (client/get (get-url day)
                              {:cookies {"session" {:value cookie}}}))
                (catch Exception e
                  (throw (IOException. (str "Error while fetching " (get-url day)) e)))))
        (throw (IllegalStateException. (str "Cannot http/get input: Missing session string (" *session-cookie-global* " or " *session-cookie-local* ")")))))
    (slurp filename)))


;;; collections functions

(defn conj-vec
  "Like `conj` but creates a vector if coll is nil."
  [coll x]
  (if coll
    (conj coll x)
    [x]))

(defn conj-set
  "Like `conj` but creates a set if coll is nil."
  [coll x]
  (if coll
    (conj coll x)
    #{x}))

;;; math functions

(defn gcd
  "Return greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Returns lowest common multiple of a and b.
  Note: This method is not tested with negative numbers."
  [a b]
  (/ (Math/abs (* a b)) (gcd a b)))
