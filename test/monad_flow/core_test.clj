(ns monad-flow.core-test
  (:require [monad-flow.core :as mf :refer [success? lift try-chain-> try-chain->> lift-> lift->>]]
            [clojure.test :refer :all])
  (:use [slingshot.slingshot :only [throw+]])
  (:import [monad_flow.core Right Left]))

(deftest test-right-and-left
  (testing "Right and Left records"
    (let [right-val (mf/->Right 42)
          left-val  (mf/->Left "error")]
      (is (true? (success? right-val)))
      (is (false? (success? left-val)))
      (is (= (:value right-val) 42))
      (is (= (:error left-val) "error")))))

(deftest lift-test
  (testing "lift simple value"
    (let [func (mf/lift identity)
          res (func 10)]
      (is (instance? Right res))
      (is (success? res))
      (is (= 10 (:value res)))))

  (testing "lift simple value multi-arity function"
    (let [func (mf/lift #(+ % %2 %3 %4 %5))
          res (func 1 2 3 4 5)]
      (is (instance? Right res))
      (is (success? res))
      (is (= 15 (:value res)))))

  (testing "wrong arity"
    (is (thrown? clojure.lang.ArityException
                 (let [func (mf/lift #(+ % %2))]
                   (func 1 2 3 4 5)))))

  (testing "return nil"
    (let [func (mf/lift (fn [] nil))
          res (func)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (= "Nil returned" (:error res)))))

  (testing "return nil multi-arity function"
    (let [func (mf/lift #(do (+ % %2 %3 %4 %5) nil))
          res (func 1 2 3 4 5)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (= "Nil returned" (:error res)))))

  (testing "throw exception"
    (let [func (mf/lift #(throw (IllegalArgumentException. "arg1")))
          res (func)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (instance? IllegalArgumentException (:error res)))))

  (testing "throw+ error"
    (let [func (mf/lift #(throw+ {:type :unit-error}))
          res (func)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (= :unit-error (:type (:error res))))))

  (testing "throw with error handler"
    (let [func (mf/lift #(throw (IllegalArgumentException. "arg1"))
                       #(str "error: " (.getMessage %)))
          res (func)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (= "error: arg1" (:error res)))))

  (testing "throw+ error with error handler"
    (let [func (mf/lift #(throw+ {:type :unit-error})
                       :type)
          res (func)]
      (is (instance? Left res))
      (is (not (success? res)))
      (is (= :unit-error (:error res))))))

(deftest try-chain->-test
  (testing "single expression"
    (let [[value :as result] (mf/try-chain-> 10)]
      (is (instance? Right result))
      (is (= 10 value)))
    (let [[value :as result] (mf/try-chain-> (assoc {} :a 10))]
      (is (instance? Right result))
      (is (map? value))
      (is (= 10 (:a value))))
    (let [[value :as result] (mf/try-chain-> (#(do 10)))]
      (is (instance? Right result))
      (is (= 10 value))))

  (testing "functions returning a value, not a vector"
    (let [[value :as result] (mf/try-chain-> 10
                                          (+ 1))]
      (is (instance? Right result))
      (is (= 11 value)))
    (let [[value :as result] (mf/try-chain-> 10
                                          (+ 1)
                                          pos?)]
      (is (instance? Right result))
      (is (= true value))))

  (testing "nil expr"
    (let [[val err :as result] (mf/try-chain-> nil
                                            (mf/lift inc)
                                            pos?)]
      (is (instance? Left result))
      (is (nil? val))
      (is (= "Nil returned" err))))

  (testing "nil form"
    (let [[val err :as result] (mf/try-chain-> 10
                                            ((mf/lift inc))
                                            (#(do (inc %) nil))
                                            (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= "Nil returned" err))))

  (testing "nil form with error"
    (let [[val error :as result] (mf/try-chain-> 10
                                              ((mf/lift inc))
                                              (#(do (inc %) [nil {:type :planned-error}]))
                                              (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= :planned-error (:type error)))))

  (testing "lifted form throws"
    (let [[val error :as result] (mf/try-chain-> 10
                                              ((mf/lift #(throw (IllegalArgumentException. (str %)))))
                                              (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (instance? IllegalArgumentException error))))

  (testing "lifted form throws+"
    (let [[val error :as result] (mf/try-chain-> 10
                                              ((mf/lift #(throw+ {:type :planned-error :p %})))
                                              (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= :planned-error (:type error)))))

  (testing "multi-arity forms"
    (let [str-int-map->int (fn [an-str an-int a-map]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-1]
                               10))
          int-map-str->map (fn [an-int a-map an-str]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-2]
                               {:another "map"}))
          map-str-int->str (fn [a-map an-str an-int]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-3]
                               "all good"))
          res (mf/try-chain-> "an str"
                           identity
                           (str-int-map->int 10 {:a "map"})
                           identity
                           (int-map-str->map {:another "map"} "other str")
                           identity
                           (map-str-int->str "last str" 10))]
      (is (instance? Right res))
      (is (= "all good" (:value res)))))

  (testing "lifted multi-arity forms"
    (let [str-int-map->int (fn [an-str an-int a-map]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-1]
                               10))
          int-map-str->map (fn [an-int a-map an-str]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-2]
                               {:another "map"}))
          map-str-int->str (fn [a-map an-str an-int]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-3]
                               "all good"))
          res (mf/try-chain-> "an str"
                           identity
                           ((mf/lift str-int-map->int) 10 {:a "map"})
                           identity
                           ((mf/lift int-map-str->map) {:another "map"} "other str")
                           identity
                           ((mf/lift map-str-int->str) "last str" 10))]
      (is (instance? Right res))
      (is (= "all good" (:value res)))))

  (testing "vectors are still vectors"
    (let [[result _] (mf/try-chain-> (mf/lift-> [1 2 3])
                                  (mf/lift-> vector?))]
      (is (= true result))))

  (testing "work with vectors"
    (let [[val err] (mf/try-chain-> [0])]
      (is (vector? val))
      (is (= 1 (count val)))
      (is (= 0 (first val))))
    (let [[val err] (mf/try-chain-> [0 1])]
      (is (vector? val))
      (is (= 2 (count val)))
      (is (= 0 (first val)))
      (is (= 1 (second val))))
    (let [[result error] (mf/try-chain-> (mf/lift-> [0])
                                      (mf/lift-> (conj 1))
                                      (mf/lift-> vec)
                                      (mf/lift-> (conj 2))
                                      (mf/lift-> vec))]
      (is (vector? result))
      (is (= 0 (first result)))
      (is (= 1 (second result)))
      (is (= 2 (nth result 2)))))

  (testing "error in first form"
    (let [[res err] (mf/try-chain-> [nil {:type :some-error}]
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))
    (let [[res err] (mf/try-chain-> (mf/lift-> [nil {:type :some-error}])
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))
    (let [[res err] (mf/try-chain-> ((mf/lift #(throw (Exception. "some error"))))
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))
    (let [[res err] (mf/try-chain-> (mf/lift-> (throw (Exception. "some error")))
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))
    (let [[res err] (mf/try-chain-> (mf/lift-> (throw+ {:type :some-error}))
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))))


(deftest try-chain->>-test
  (testing "single expression"
    (let [[value :as result] (mf/try-chain->> 10)]
      (is (instance? Right result))
      (is (= 10 value)))
    (let [[value :as result] (mf/try-chain->> (assoc {} :a 10))]
      (is (instance? Right result))
      (is (map? value))
      (is (= 10 (:a value))))
    (let [[value :as result] (mf/try-chain-> (#(do 10)))]
      (is (instance? Right result))
      (is (= 10 value))))

  (testing "functions returning a value, not a vector"
    (let [[value :as result] (mf/try-chain->> 10
                                           (+ 1))]
      (is (instance? Right result))
      (is (= 11 value)))
    (let [[value :as result] (mf/try-chain->> 10
                                           (+ 1)
                                           pos?)]
      (is (instance? Right result))
      (is (= true value))))

  (testing "nil expr"
    (let [[val err :as result] (mf/try-chain->> nil
                                             (mf/lift inc)
                                             pos?)]
      (is (instance? Left result))
      (is (nil? val))
      (is (= "Nil returned" err))))

  (testing "nil form"
    (let [[val err :as result] (mf/try-chain->> 10
                                             ((mf/lift inc))
                                             (#(do (inc %) nil))
                                             (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= "Nil returned" err))))

  (testing "nil form with error"
    (let [[val error :as result] (mf/try-chain->> 10
                                               ((mf/lift inc))
                                               (#(do (inc %) [nil {:type :planned-error}]))
                                               (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= :planned-error (:type error)))))

  (testing "lifted form throws"
    (let [[val error :as result] (mf/try-chain->> 10
                                               ((mf/lift #(throw (IllegalArgumentException. (str %)))))
                                               (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (instance? IllegalArgumentException error))))

  (testing "lifted form throws+"
    (let [[val error :as result] (mf/try-chain->> 10
                                               ((mf/lift #(throw+ {:type :planned-error :p %})))
                                               (#(throw+ {:type :shouldnt-be-here :p %})))]
      (is (instance? Left result))
      (is (nil? val))
      (is (= :planned-error (:type error)))))

  (testing "multi-arity forms"
    (let [int-map-str->int (fn [an-int a-map an-str]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-1]
                               10))
          map-str-int->map (fn [a-map an-str an-int]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-2]
                               {:another "map"}))
          str-int-map->str (fn [an-str an-int a-map]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-3]
                               "all good"))
          res (mf/try-chain->> "an str"
                            identity
                            (int-map-str->int 10 {:a "map"})
                            identity
                            (map-str-int->map {:another "map"} "other str")
                            identity
                            (str-int-map->str "last str" 10))]
      (is (instance? Right res))
      (is (= "all good" (:value res)))))

  (testing "lifted multi-arity forms"
    (let [int-map-str->int (fn [an-int a-map an-str]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-1]
                               10))
          map-str-int->map (fn [a-map an-str an-int]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-2]
                               {:another "map"}))
          str-int-map->str (fn [an-str an-int a-map]
                             (if (not (and (string? an-str) (int? an-int) (map? a-map)))
                               [nil :invalid-types-3]
                               "all good"))
          res (mf/try-chain->> "an str"
                            identity
                            ((mf/lift int-map-str->int) 10 {:a "map"})
                            identity
                            ((mf/lift map-str-int->map) {:another "map"} "other str")
                            identity
                            ((mf/lift str-int-map->str) "last str" 10))]
      (is (instance? Right res))
      (is (= "all good" (:value res)))))

  (testing "vectors are still vectors"
    (let [[result _] (mf/try-chain->> (mf/lift->> [1 2 3])
                                   (mf/lift->> vector?))]
      (is (= true result))))

  (testing "work with vectors"
    (let [[val err] (mf/try-chain->> [0])]
      (is (vector? val))
      (is (= 1 (count val)))
      (is (= 0 (first val))))
    (let [[val err] (mf/try-chain->> [0 1])]
      (is (vector? val))
      (is (= 2 (count val)))
      (is (= 0 (first val)))
      (is (= 1 (second val))))
    (let [[result error] (mf/try-chain->> (mf/lift->> [0])
                                       (mf/lift->> (cons 1))
                                       (mf/lift->> vec)
                                       (mf/lift->> (cons 2))
                                       (mf/lift->> vec))]
      (is (vector? result))
      (is (= 2 (first result)))
      (is (= 1 (second result)))
      (is (= 0 (nth result 2)))))

  (testing "error in first form"
    (let [[res err] (mf/try-chain->> [nil {:type :some-error}]
                                  inc
                                  pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))
    (let [[res err] (mf/try-chain->> (mf/lift->> [nil {:type :some-error}])
                                  inc
                                  pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))
    (let [[res err] (mf/try-chain-> ((mf/lift #(throw (Exception. "some error"))))
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))
    (let [[res err] (mf/try-chain->> (mf/lift->> (throw (Exception. "some error")))
                                  inc
                                  pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))
    (let [[res err] (mf/try-chain->> (mf/lift->> (throw+ {:type :some-error}))
                                  inc
                                  pos?)]
      (is (nil? res))
      (is (= :some-error (:type err))))))


(deftest lift->-test
  (testing "lift single arg fn"
    (let [[res err] (mf/try-chain-> {:a "a value"}
                                 (mf/lift-> :a))]
      (is (= res "a value"))))

  (testing "lift anonymous fn"
    (let [[res err] (mf/try-chain-> {:a "a value"}
                                 (mf/lift-> (#(:a %))))]
      (is (= "a value" res))))

  (testing "lift multi-arg fn"
    (let [[res error] (mf/try-chain-> 15
                                   (mf/lift-> inc)
                                   (mf/lift-> (/ 2 2 2 2)))]
      (is (= 1 res))))

  (testing "lift with ->"
    (let [res (-> {}
                  (mf/lift-> (assoc :value "a value"))
                  (nth 0)                                   ; value
                  :value)]
      (is (= "a value" res))))

  (testing "error in first form"
    (let [[res err] (mf/try-chain-> (mf/lift-> (throw (Exception. "some error")))
                                 inc
                                 pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))))


(deftest lift->>-test
  (testing "lift single arg fn"
    (let [[res err] (mf/try-chain->> {:a "a value"}
                                  (mf/lift->> :a))]
      (is (= res "a value"))))

  (testing "lift anonymous fn"
    (let [[res err] (mf/try-chain->> {:a "a value"}
                                  (mf/lift->> (#(:a %))))]
      (is (= "a value" res))))

  (testing "lift multi-arg fn"
    (let [[res error] (mf/try-chain->> 1
                                    (mf/lift->> inc)
                                    (mf/lift->> (/ 16 2 2 2)))]
      (is (= 1 res))))

  (testing "lift with ->>"
    (let [res (->> "a value"
                   (mf/lift->> (assoc {} :value))
                   :value
                   :value)]
      (is (= "a value" res))))

  (testing "error in first form"
    (let [[res err] (mf/try-chain->> (mf/lift->> (throw (Exception. "some error")))
                                  inc
                                  pos?)]
      (is (nil? res))
      (is (= "some error" (.getMessage err))))))

(deftest inspect-chain->-test
  (testing "basic"
    (let [[val] (mf/try-chain-> 10
                             (mf/inspect-chain-> (println "initial value: " :flow-value))
                             inc
                             (mf/inspect-chain-> (println "intermediate value: " :flow-value))
                             inc
                             (mf/inspect-chain-> (do (println "not important") :return-something))
                             inc
                             (mf/inspect-chain-> (println "final value: " :flow-value)))]
      (is (= val 13)))
    (testing "with state"
      (let [state (atom {})
            [val] (mf/try-chain-> 10
                               (mf/inspect-chain-> (println "initial value: " :flow-value))
                               inc
                               (mf/inspect-chain-> (swap! state #(assoc % :the-value :flow-value)))
                               inc)]
        (is (= 11 (:the-value @state)))
        (is (= 12 val))))))

(deftest inspect-chain->>-test
  (testing "basic"
    (let [[val] (mf/try-chain->> 10
                              (mf/inspect-chain->> (println "initial value: " :flow-value))
                              inc
                              (mf/inspect-chain->> (println "intermediate value: " :flow-value))
                              inc
                              (mf/inspect-chain->> (do (println "not important") :return-something))
                              inc
                              (mf/inspect-chain->> (println "final value: " :flow-value)))]
      (is (= val 13))))
  (testing "with state"
    (let [state (atom {})
          [val] (mf/try-chain->> 10
                              (mf/inspect-chain->> (println "initial value: " :flow-value))
                              inc
                              (mf/inspect-chain->> (swap! state #(assoc % :the-value :flow-value)))
                              inc)]
      (println @state)
      (is (= 11 (:the-value @state)))
      (is (= 12 val)))))
