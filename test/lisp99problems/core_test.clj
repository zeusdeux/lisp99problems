(ns lisp99problems.core-test
  (:require [clojure.test :refer :all]
            [lisp99problems.core :refer :all]))

(deftest a-test
  (testing "1 is 1"
    (is (= 1 1))))

(deftest sum-test
  (testing "sum of [] is 0"
    (is (= 0 (lisp99problems.core/sum []))))
  (testing "sum of vector with one element is the element itself"
    (is (= 1 (lisp99problems.core/sum [1]))))
  (testing "sum of a vector is the sum of its elements"
    (is (= 10 (lisp99problems.core/sum [1 2 3 4])))))
