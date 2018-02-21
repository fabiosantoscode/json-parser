(ns json-parser.core
  (:require [clojure.string :refer [split]]
            [clojure.math.numeric-tower :as math]))

(def token-re
  #"(?:([:,\[\]{}])|(true|false)|(-?(?:[0-9]\d*\.?|0\.)\d*(?:[eE][+-]?\d+)?)|(\"(?:[^\"\\]|\\[\"\\/bfnrt]|\\u[0-9a-fA-F]{4})+?\"))")

(def class-punc 1)
(def class-bool 2)
(def class-number 3)
(def class-string 4)

(defn tokenise
  "Turns a string of JSON into a seq of matches. Matches are vectors indexed by the class-* variables in this module."
  [string]
  (re-seq token-re string))

(defn get-class [v]
  (if
    (some? (nth v class-punc)) class-punc
    (if
      (some? (nth v class-bool)) class-bool
      (if
        (some? (nth v class-number)) class-number
        class-string))))

(defn- parse-bool [s]
  (if (= s "true")
    true
    false))

(defn- parse-number [s]
  (def neg (if (= (nth s 0) \-) -1 1))
  (def int-string (nth (re-find #"^-?(\d+)" s) 1))
  (def dec-string (nth (re-find #"\.(\d+)?" s) 1))
  (def exp-string (nth (re-find #"[eE](\d+)" s) 1))
  (def int-part (Integer. int-string))
  (def dec-part
    (if dec-string
      (/
       (Integer. dec-string)
       (math/expt 10 (count dec-string))
       1.0)
      0))
  (def exponential-part
    (if exp-string
      (Integer. exp-string)
      0))
  (def base (+ (* neg int-part) dec-part))
  (* base (math/expt 10 exponential-part)))

(defn- slash-escapes [c]
  (case c
    \b "\b"
    \f "\f"
    \n "\n"
    \r "\r"
    \t "\t"
    \\ "\\"
    \" "\""))

(defn- parse-string-part [s]
  (if (= s "")
    ""
    (if (= (first s) \\)
      (str (slash-escapes (second s)) (parse-string-part (subs s 2)))
      (str (subs s 0 1) (parse-string-part (subs s 1))))))

(defn- parse-string [s]
  (def string-part (nth (re-find #"^(?:\")(.*?)(?:\")$" s) 1))
  (parse-string-part string-part))

(defn- is-punc [token punc]
  (= (nth token class-punc) punc))

(defn- expect-punc [tokens punc]
  (assert (is-punc (first tokens) punc))
  (rest tokens))

(defn- expect-comma [tokens start-collection not-first]
  (if not-first
    (expect-punc tokens ",")
    (expect-punc tokens start-collection)))

(declare parse-tok)

(defn- parse-array
  ([tokens] (parse-array tokens false))
  ([tokens not-first]
   (let [tokens (expect-comma tokens "[" not-first)]
     (if (is-punc (first tokens) "]")
       [(rest tokens) []]
       (let [[tokens value] (parse-tok tokens) ]
         (if (is-punc (first tokens) ",")
           (let [[tokens rest-of-array] (parse-array tokens true)]
             [tokens (concat [value] rest-of-array)])
           [(rest tokens) [value]]))))))

(defn parse-object
  ([tokens] (parse-object tokens false))
  ([tokens not-first]
   (let [tokens (expect-comma tokens "{" not-first)]
     (if (is-punc (first tokens) "}")
       [(rest tokens) {}]
       (let [[tokens key] (parse-tok tokens)
             tokens (expect-punc tokens ":")
             [tokens value] (parse-tok tokens)]
         (if (is-punc (first tokens) ",")
           (let [[tokens rest-of-object] (parse-object tokens true)]
             [tokens (merge {key value} rest-of-object)])
           [(rest tokens) {key value}]))))))

(defn parse-tok [tokens]
  (let [cur (first tokens)
        cls (get-class cur)]
    ;(println "parse-tok:" cls " " cur)
    (cond
      (is-punc cur "[") (parse-array tokens)
      (is-punc cur "{") (parse-object tokens)
      (= cls class-punc) (assert false "unreachable")
      (= cls class-bool) [(rest tokens) (parse-bool (nth cur class-bool))]
      (= cls class-number) [(rest tokens) (parse-number (nth cur class-number))]
      (= cls class-string) [(rest tokens) (parse-string (nth cur class-string))])))


(defn parse
  "Parses a string as JSON"
  [string]
  (def tokens (tokenise string))
  (let [[tokens value] (parse-tok tokens)]
    (assert (empty? tokens))
    value))
