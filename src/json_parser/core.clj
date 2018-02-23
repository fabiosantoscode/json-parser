(ns json-parser.core
  (:require [clojure.string :refer [split]]
            [clojure.math.numeric-tower :as math]))

(def token-re
  #"(?:([:,\[\]{}])|(true|false)|(-?(?:[0-9]\d*\.?|0\.)\d*(?:[eE][+-]?\d+)?)|(\"(?:[^\"\\]|\\[\"\\/bfnrt]|\\u[0-9a-fA-F]{4})+?\")|(\s+)|(.+?))")

(def class-punc 1)
(def class-bool 2)
(def class-number 3)
(def class-string 4)
(def class-whitespace 5)
(def class-syntax-error 6)

(defn get-class [token]
  (cond
    (some? (nth token class-punc)) class-punc
    (some? (nth token class-bool)) class-bool
    (some? (nth token class-number)) class-number
    (some? (nth token class-string)) class-string
    (some? (nth token class-whitespace)) class-whitespace
    (some? (nth token class-syntax-error)) class-syntax-error
    :else (assert false "unreachable")))

(defn unexpected-tok [token]
  (throw (Exception. (str "Unexpected: '" (nth token (get-class token)) "'"))))

(defn tokenise
  "Turns a string of JSON into a seq of matches. Matches are vectors indexed by the class-* variables in this module. Drops whitespace"
  [string]
  (for [token (re-seq token-re string)
        :when (not= (get-class token) class-whitespace)]
    (if (= (get-class token) class-syntax-error)
      (unexpected-tok token)
      token)))

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
  (assert (is-punc (first tokens) punc) (str "Expected '" punc "'"))
  (rest tokens))

(declare parse-atom)

(defn- parse-array [tokens]
  (loop [tokens (expect-punc tokens "[")
         accum []]
    (if (is-punc (first tokens) "]")
      [(rest tokens) []]
      (let [[tokens value] (parse-atom tokens)
            accum (conj accum value)]
        (if-not (is-punc (first tokens) ",")
          [(expect-punc tokens "]") accum]
          (recur (rest tokens) accum))))))

(defn- parse-object [tokens]
  (loop [tokens (expect-punc tokens "{")
         accum {}]
    (if (is-punc (first tokens) "}")
      [(rest tokens) accum]
      (let [[tokens key] (parse-atom tokens)
            tokens (expect-punc tokens ":")
            [tokens value] (parse-atom tokens)
            accum (merge {key value} accum)]
        (if-not (is-punc (first tokens) ",")
          [(expect-punc tokens "}") accum]
          (recur (rest tokens) accum))))))

(defn parse-atom [tokens]
  (let [cur (first tokens)
        cls (get-class cur)]
    ;(println "parse-tok:" cls " " cur)
    (cond
      (is-punc cur "[") (parse-array tokens)
      (is-punc cur "{") (parse-object tokens)
      (= cls class-punc) (assert false "unreachable")
      (= cls class-bool) [(rest tokens) (parse-bool (nth cur class-bool))]
      (= cls class-number) [(rest tokens) (parse-number (nth cur class-number))]
      (= cls class-string) [(rest tokens) (parse-string (nth cur class-string))]
      :else (assert false "unreachable"))))

(defn parse
  "Parses a string as JSON"
  [string]
  (let [tokens (tokenise string)
        [tokens value] (parse-atom tokens)]
    (assert (empty? tokens))
    value))
