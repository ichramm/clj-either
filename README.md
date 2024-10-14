# monad-flow
[![Build Status](https://travis-ci.org/ichramm/monad-flow.svg?branch=master)](https://travis-ci.org/ichramm/monad-flow)
[![codecov](https://codecov.io/gh/ichramm/monad-flow/branch/master/graph/badge.svg)](https://codecov.io/gh/ichramm/monad-flow)
[![Clojars Project](https://img.shields.io/clojars/v/com.ichramm/monad-flow.svg)](https://clojars.org/com.ichramm/monad-flow)

A Clojure library designed to provide idiomatic program flow and error handling using the Either and Continuation monads.

```clj
[monad-flow "0.0.1"]
```

## Usage

### `try-chain->` Macro

The `try-chain->` macro allows you to chain operations that may fail, returning an `Either` monad (`Right` for success, `Left` for failure).

#### Example

```clj
(require '[monad-flow.core :refer [try-chain-> Right Left]])

(defn safe-divide [x y]
  (if (zero? y)
    (->Left "Division by zero")
    (->Right (/ x y))))

(def result
  (try-chain-> 10
    (safe-divide 2)
    (safe-divide 5)))

(println result) ; => Right{:value 1}
```

### `try-chain->>` Macro

The `try-chain->>` macro is similar to `try-chain->` but threads the value through the last position of each form.

#### Example

```clj
(require '[monad-flow.core :refer [try-chain->> Right Left]])

(defn safe-divide [x y]
  (if (zero? y)
    (->Left "Division by zero")
    (->Right (/ x y))))

(def result
  (try-chain->> 10
    (safe-divide 2)
    (safe-divide 5)))

(println result) ; => Right{:value 1}
```

### `inspect-chain->` and `inspect-chain->>` Macros

These macros allow you to insert side-effecting operations (like logging) into a `try-chain` flow for debugging purposes.

#### Example

```clj
(require '[monad-flow.core :refer [try-chain-> inspect-chain->]])

(def result
  (try-chain-> 10
    (inspect-chain-> (println "initial value:" :flow-value))
    inc
    (inspect-chain-> (println "intermediate value:" :flow-value))
    inc
    (inspect-chain-> (println "final value:" :flow-value))))

; Output:
; initial value: 10
; intermediate value: 11
; final value: 12
; => Right{:value 12}
```

## License

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
