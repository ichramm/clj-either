(ns monad-flow.core
  "MonadFlow: A library for idiomatic error handling using the Either monad.
   Functions should return a vector [val error], where `val` represents a successful result 
   (the Right value), and `error` represents a failure (the Left value). If no error occurs, 
   `val` contains the result, and `error` is nil. Functions not following this convention 
   can be 'lifted' using the `lift` function."
  (:use [slingshot.slingshot :only [try+ throw+]]))

(defprotocol Either
  "The Either monad interface, representing success (Right) or error (Left)."
  (success? [_] "Returns true if this is a Right value, indicating success."))

(defrecord Right [value]
  Either
  (success? [_] true)

  ; support destructuring: (let [[val err] (try-chain->....)] ...)
  clojure.lang.Indexed
  (nth [_ i]
    (case i 0 value 1 nil (throw (IndexOutOfBoundsException.))))
  (nth [_ i default]
    (case i 0 value 1 nil default))

  Object
  (toString [_]
    (format "Right{:value %s}" value)))

(defrecord Left [error]
  Either
  (success? [_] false)

  ; support destructuring: (let [[val err] (try-chain->....)] ...)
  clojure.lang.Indexed
  (nth [_ i]
    (case i 0 nil 1 error (throw (IndexOutOfBoundsException.))))
  (nth [_ i default]
    (case i 0 nil 1 error default))

  Object
  (toString [_]
    (format "Left{:error %s}" error)))

(defn lift-value
  "Wraps the plain value `x` into a monadic (Right, Left) value. 
   If `x` is nil or a [nil error] pair, it returns a Left with an error message."
  [x]
  (cond
    (nil? x) (->Left "Nil returned")
    (satisfies? Either x) x
    (and (vector? x) (= 2 (count x)) (nil? (first x))) (->Left (nth x 1))
    :else (->Right x)))

(defn lift
  "Wraps a function `f` with error handling, returning a monadic value 
   (Right on success, Left on error). Optionally, customize the error transformation with `error-fn`."
  ([f]
   (lift f identity))
  ([f error-fn]
   (fn [& args]
     (try+
       (let [val (apply f args)]
         (lift-value val))
       (catch clojure.lang.ArityException _
         (throw+))
       (catch Object e
         (->Left (error-fn e)))))))

(def
  ^{:doc
    "Alias for `lift`"
    :arglists '([f] [f error-fn])}
  wrap lift)

(defmacro try-chain->
  "Attempts to thread the expression `expr` through forms, like `->`, but handling errors monadically.
  Each function must return an Either value or a pair [val error] pair. If an error occurs (Left), it short-circuits the flow.

  Example:
     (try-chain-> 1
       (wrap inc))
     => Right{:value 2}

     (try-chain-> 1
       (wrap inc)
       (wrap #(throw+ (str \"error: \" %))))
     => Left{:error \"error: 2\"}"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step]
                     `(if (success? ~g) 
                        (-> (:value ~g) ~step lift-value) 
                        ~g))
                   forms)]
    `(let [~g (lift-value ~expr)
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro try-chain->>
  "Similar to `try-chain->`, but uses the `->>` threading model. The result of each function is 
   passed as the last argument to the next form. Returns either a Right (success) or Left (error).
   
   Example:
     (try-chain->> 1
       (wrap inc)
       (wrap #(throw+ (str \"error: \" %))))
     => Left{:error \"error: 2\"}"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step]
                     `(if (success? ~g) 
                        (->> (:value ~g) ~step lift-value) 
                        ~g))
                   forms)]
    `(let [~g (lift-value ~expr)
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))


(defmacro lift->
  "Helper for more readable error handling inside `try-chain->`.
   Automatically wraps expressions in a try block and returns a monadic value.

  Example:
     (try-chain-> {:a \"a value\"}
       (lift-> :a))
     => Right{:value \"a value\"}"
  ([body]
   `((lift (fn [] ~body))))
  ([val body]
   `((lift (fn []
             ~(if (seq? body)
                (with-meta `(~(first body) ~val ~@(next body)) (meta body))
                (list body val)))))))

(defmacro lift->>
  "Helper for more readable error handling inside `try-chain->>`.
   Automatically wraps expressions in a try block and returns a monadic value.
   
   Example:
     (try-chain->> {:a \"a value\"}
       (lift->> :a))
     => Right{:value \"a value\"}"
  ([body]
   `((lift (fn [] ~body))))
  ([body val]
   `((lift (fn []
             ~(if (seq? body)
                (with-meta `(~(first body) ~@(next body) ~val) (meta body))
                (list body val)))))))

(defn- deep-replace
  "Recursively replaces values in a nested collection using the substitution map `smap`."
  [smap coll]
  (if (seq? coll)
    (doall (map (fn [e]
                  (if-let [entry (find smap e)]
                    (val entry)
                    (if (seq? e)
                      (deep-replace smap e)
                      e)))
                coll))
    (if-let [entry (find smap coll)]
      (val entry)
      coll)))

(defmacro inspect-chain->
  "Inserts side-effecting operations into a `try-chain->` flow for debugging or logging.
   The value `:flow-value` is replaced with the current computation value.

  Example:
    (f/try-chain-> 10
                 (f/inspect-chain-> (println \"initial value: \" :flow-value))
                 inc
                 (f/inspect-chain-> (println \"intermediate value: \" :flow-value))
                 inc
                 (f/inspect-chain-> (println \"not important\"))
                 inc
                 (f/inspect-chain-> (println \"final value: \" :flow-value)))
    initial value:  10
    intermediate value:  11
    not important
    final value:  13
    => Right{:value 13}"
  [val body]
  `('do ~(deep-replace {:flow-value val} body) ~val))

(defmacro inspect-chain->>
  "Inserts side-effecting operations into a `try-chain->>` flow for debugging or logging.
   The value `:flow-value` is replaced with the current computation value.

  Example:
    (f/try-chain->> 10
                  (f/inspect-chain->> (println \"initial value: \" :flow-value))
                  inc
                  (f/inspect-chain->> (println \"intermediate value: \" :flow-value))
                  inc
                  (f/inspect-chain->> (println \"not important\"))
                  inc
                  (f/inspect-chain->> (println \"final value: \" :flow-value)))
    initial value:  10
    intermediate value:  11
    not important
    final value:  13
    => .Right{:value 13}"
  [body val]
  `(inspect-chain-> ~val ~body))
