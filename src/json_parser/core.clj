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
(def class-eof 7)

(defrecord Token [value cls line col])

(defn- match-to-token [match line col]
  (def cls (cond
      (some? (nth match class-punc)) class-punc
      (some? (nth match class-bool)) class-bool
      (some? (nth match class-number)) class-number
      (some? (nth match class-string)) class-string
      (some? (nth match class-whitespace)) class-whitespace
      (some? (nth match class-syntax-error)) class-syntax-error
      :else (assert false "unreachable")))
  (def value (nth match cls))
  (->Token value cls line col))

(defn- is-punc [token punc]
  (and (= (:value token) punc) (= (:cls token) class-punc)))

(defn- at-string [token]
  (str "at " (:line token) ":" (:col token)))

(defn- token-error-string [token]
  (if (= (:cls token) class-eof)
    "end of input"
    (str "'" (:value token) "'")))

(defn- unexpected-tok
  ([token] (unexpected-tok token nil))
  ([token message]
    (def message-string (if message (str ". " message) ""))
    (throw (Exception. (str
                         "Unexpected: "
                         (token-error-string token)
                         " "
                         (at-string token)
                         message-string)))))

(defn- expect [tokens cls value message]
  (let [[cur & remaining] tokens]
    (if (and (= (:cls cur) cls) (= (:value cur) value))
      remaining
      (unexpected-tok cur message))))

(defn- expect-punc [tokens punc]
  (expect tokens class-punc punc (str "Expected '" punc "'")))

(defn- expect-eof [tokens]
  (expect tokens class-eof "EOF" (str "Expected end of input")))

(defn- matches-to-tokens
  ([tokens] (matches-to-tokens tokens 1 1))
  ([tokens line col]
   (defn count-newlines [s] (count (filter #(= % \newline) s)))
   (lazy-seq
     (if-not (seq tokens)
       (seq [(->Token "EOF" class-eof line col)])
       (let [token (match-to-token (first tokens) line col)
             prev-line line
             line (+ line (count-newlines (:value token)))
             col (if (not= prev-line line)
                   (+ 1 (count (last (split (:value token) #"\n"))))
                   (+ col (count (:value token))))]
         (cons token (matches-to-tokens (rest tokens) line col)))))))

(defn tokenise
  "Turns a string of JSON into a seq of matches. Matches are vectors indexed by the class-* variables in this module. Drops whitespace"
  [string]
  (for [token (matches-to-tokens (re-seq token-re string))
        :when (not= (:cls token) class-whitespace)]
    (if (= (:cls token) class-syntax-error)
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

(defn- parse-atom [tokens]
  (let [cur (first tokens)
        cls (:cls cur)]
    ;(println "parse-tok:" cls " " cur)
    (cond
      (is-punc cur "[") (parse-array tokens)
      (is-punc cur "{") (parse-object tokens)
      (= cls class-bool) [(rest tokens) (parse-bool (:value cur))]
      (= cls class-number) [(rest tokens) (parse-number (:value cur))]
      (= cls class-string) [(rest tokens) (parse-string (:value cur))]
      :else (unexpected-tok cur "Expected atom"))))

(defn parse
  "Parses a string as JSON"
  [string]
  (let [tokens (tokenise string)
        [tokens value] (parse-atom tokens)]
    (expect-eof tokens)
    value))
