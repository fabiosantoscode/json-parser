(ns json-parser.core-test
  (:require [clojure.test :refer :all]
            [json-parser.core :refer :all]))

(defn tokenise-t [cls string]
  (let [tok (first (tokenise string))]
    (assert (= cls (:cls tok)))
    (:value (first (tokenise string)))))

(deftest tokenise-test
  (testing "tokenising bools"
    (is (= (tokenise-t class-bool "false") "false")))
  (testing "tokenising strings"
    (is (= (tokenise-t class-string "\"foo\"") "\"foo\""))
    (is (= (tokenise-t class-string "\"f\\\"oo\"") "\"f\\\"oo\""))
    (is (= (tokenise-t class-string "\"\\b\\f\\n\\r\\t\"") "\"\\b\\f\\n\\r\\t\""))
    (is (= (tokenise-t class-string "\"\\uFFFF\"") "\"\\uFFFF\"")))
  (testing "tokenising numbers"
    (is (= (tokenise-t class-number "1") "1"))
    (is (= (tokenise-t class-number "1.1") "1.1"))
    (is (= (tokenise-t class-number "0.1") "0.1"))
    (is (= (tokenise-t class-number "0.1e10") "0.1e10")))
  (testing "tokens contain line and column information"
    (let [[token1 token2] (tokenise "1\n  2")]
      (is (= [(:line token1) (:col token1)] [1 1]))
      (is (= [(:line token2) (:col token2)] [2 3])))))

(deftest parse-test
  (testing "parsing booleans"
    (is (= (parse "false") false)))
  (testing "parsing numbers"
    (is (= (parse "123") 123))
    (is (= (parse "-123") -123))
    (is (= (parse "0.11") 0.11))
    (is (= (parse "1.0e3") 1e3))
    (is (= (parse "1e3") 1000)))
  (testing "parsing strings"
    (is (= (parse "\"123\"") "123"))
    (is (= (parse "\"\\b\\f\\n\\r\\t\\\\\\\"\"") "\b\f\n\r\t\\\"")))
  (testing "parsing arrays"
    (is (= (parse "[]") []))
    (is (= (parse "[1]") [1]))
    (is (= (parse "[1,2]") [1 2]))
    (is (= (parse "[1,[2]]") [1 [2]])))
  (testing "parsing objects"
    (is (= (parse "{}") {}))
    (is (= (parse "{\"foo\":1}") {"foo" 1}))
    (is (= (parse "{\"foo\":1,\"bar\":2}") {"foo" 1 "bar" 2})))
  (testing "whitespace is allowable"
    (is (= (parse " 1") 1))
    (is (= (parse "1 ") 1))
    (is (= (parse "\n1") 1))
    (is (= (parse "1\n") 1))
    (is (= (parse " { \"foo\" : 1 } ") {"foo" 1}))))

(deftest syntax-error-test
  (testing "trash before or after content"
    (is (thrown-with-msg? Exception #"Unexpected: '♥' at 1:4$" (parse "124♥")))
    (is (thrown-with-msg? Exception #"Unexpected: 'x' at 1:1$" (parse "x124"))))
  (testing "unfinished arrays and objects"
    (is (thrown-with-msg? Exception #"Unexpected: end of input at 1:2. Expected atom" (parse "[")))
    (is (thrown-with-msg? Exception #"Unexpected: end of input at 1:4. Expected atom" (parse "[1,")))
    (is (thrown-with-msg? Exception #"Unexpected: end of input at 1:2. Expected atom" (parse "{"))))
  (testing "JSON after end"
    (is (thrown-with-msg? Exception #"Unexpected: '\[' at 1:3\. Expected end of input" (parse "[][")))))
