(ns com.inferstructure.repl-test
  (:require [clojure.test :as t]
            [clojure.test.check.clojure-test :as tct]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.inferstructure.repl :refer :all]))

(t/deftest under-vector
  (t/testing "vector under threshold"
    (let [v [1 2 3]]
      (t/is (= (explore 4 1 v)
               v)))))

(t/deftest over-vector
  (t/testing "vector over threshold"
    (let [v [1 2 3 4]
          ev1 (explore 4 1 v)
          ev2 (explore 4 2 v)]
      (t/is (= (count ev1) 2))
      (t/is (= ev1 [1 "3 more entries"]))
      (t/is (= ev2 [1 2 "2 more entries"])))))

(t/deftest under-map
  (t/testing "map under threshold"
    (let [m {1 1 2 2 3 3}]
      (t/is (= (explore 4 1 m)
               m)))))

(t/deftest over-map
  (t/testing "map over threshold"
    (let [m {1 1 2 2 3 3 4 4}
          em1 (explore 4 1 m)
          em2 (explore 4 2 m)]
      (t/is (= (count em1) 2))
      (t/is (contains? em1 :more))
      (t/is (= (em1 :more) "3 more entries"))
      (t/is (= (count em2) 3))
      (t/is (contains? em2 :more))
      (t/is (= (em2 :more) "2 more entries")))))

(t/deftest under-set
  (t/testing "set under threshold"
    (let [v #{1 2 3}]
      (t/is (= (explore 4 1 v)
               v)))))

(t/deftest over-set
  (t/testing "set over threshold"
    (let [s #{1 2 3 4}
          es1 (explore 4 1 s)
          es2 (explore 4 2 s)]
      (t/is (= (count es1) 2))
      (t/is (contains? es1 "3 more entries"))
      (t/is (= (count es2) 3))
      (t/is (contains? es2 "2 more entries")))))

(t/deftest under-seq
  (t/testing "seq under threshold"
    (let [s (list 1 2 3)]
      (t/is (= (explore 4 1 s)
               s)))))

(t/deftest over-seq
  (t/testing "finite seq over threshold"
    (let [s (list 1 2 3 4)
          es1 (explore 4 1 s)
          es2 (explore 4 2 s)]
      (t/is (= es1 '(1 "more ...")))
      (t/is (= es2 '(1 2 "more ..."))))))

(t/deftest inf-seq
  (t/testing "infinite seq"
    (let [s (range)
          es1 (explore 10 1 s)
          es2 (explore 10 2 s)]
      (t/is (= es1 '(0 "more ...")))
      (t/is (= es2 '(0 1 "more ..."))))))

(tct/defspec small-vector
             100
             (prop/for-all [v (gen/vector (gen/choose 0 9) 1 10)]
                           (= (explore 11 1 v) v)))

(tct/defspec large-vector
             100
             (prop/for-all [p (gen/choose 1 5)
                            v (gen/vector (gen/choose 0 9) 11 100)]
                           (let [ev (explore 11 p v)
                                 cev (count ev)]
                             (= (conj (into [] (take p v)) (str (- (count v) p) " more entries"))
                                ev))))
