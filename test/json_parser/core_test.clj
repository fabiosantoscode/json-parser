(ns json-parser.core-test
  (:require [clojure.test :refer :all]
            [json-parser.core :refer :all]))

(defn tokenise-t [cls string]
  (nth (first (tokenise string)) cls))

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
    (is (= (tokenise-t class-number "0.1e10") "0.1e10"))))


(deftest get-class-test
  (testing "get-class"
    (is (= (get-class [nil nil nil nil ""]) class-string))))

(deftest bool-test
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
    (is (= (parse "{\"foo\":1,\"bar\":2}") {"foo" 1 "bar" 2}))))
