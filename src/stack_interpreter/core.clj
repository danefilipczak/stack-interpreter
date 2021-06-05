(ns stack-interpreter.core
  (:require [clojure.string :as s]))

;; HELPERS
(defmacro spy [expr]
  `(do (println ~expr "\uD83D\uDC40 ")
       ~expr))

(defn safe-pop [stack]
  (cond-> stack
    (not-empty stack)
    pop))

;; TOKENS
;; The set of valid statements in the language

(defrecord VariableToken [symbol])

(defrecord VariableDeclarationToken [symbol])

(defrecord PopToken [])

(defrecord ConstantToken [constant])

(defrecord InvokeToken [op airity])

(defrecord IfElseToken [true-clause false-clause])

;; READERS
;; Read data and return a valid Token, or throw if invalid

(defprotocol IRead
  (read [token]))

(defn variable? [symbol]
  (and (s/starts-with? (name symbol) "!")
       (> (-> symbol name (s/replace "+" "") count) 1)))

(defn variable-declaration? [symbol]
  (and (variable? symbol)
       (s/ends-with? (name symbol) "+")))

(extend-protocol IRead
  clojure.lang.Symbol
  (read [sym]
    (cond
      (= sym '<pop>)
      (PopToken.)

      (variable-declaration? sym)
      (VariableDeclarationToken. (->> sym ;; drop trailing '+'
                                      name
                                      butlast
                                      (apply str)
                                      symbol))

      (variable? sym)
      (VariableToken. sym)
      :else (throw (ex-info "Tried to parse invalid symbol" {:symbol sym})))))

(defn invocation? [list]
  (and
    (= (count list) 3)
    (= (first list) 'invoke>)
    (ifn? (second list))
    (nat-int? (last list))))

(defn parse-if-else-list [list]
  (let [[[head & true-clause] _ false-clause] (->> list
                                                   (partition-by (partial = (quote else>))))]
    (when (and (= head (quote if>))
               (seq true-clause)
               (seq false-clause))
      [true-clause false-clause])))

(extend-protocol IRead
  clojure.lang.IPersistentList
  (read [list]
    (let [[true-clause false-clause :as clauses] (parse-if-else-list list)]
      (cond
        clauses (IfElseToken. true-clause false-clause)
        (invocation? list) (InvokeToken. (second list) (last list))
        :else (throw (ex-info "Tried to parse invalid list" {:list list}))))))

(extend-protocol IRead
  Object
  (read [constant]
    (ConstantToken. constant)))

;; EVALUATION
;; receive a Token and an interpreter state and update the state according to the Token

(defprotocol IEval
  (eval [token state]))

(def empty-state {:bindings {}
                  :stack []})

(defn read-eval [clause state]
  (->> clause
       (map read)
       (reduce
         (fn [prev-state token]
           (eval token prev-state))
         state)))

(extend-protocol IEval
  VariableToken
  (eval [{symbol :symbol} {:keys [bindings stack] :as state}]
    (if-let [value (get bindings symbol)]
      (update state :stack conj value)
      (throw (ex-info (str "Use of undeclared Var " symbol) {:var symbol}))))

  VariableDeclarationToken
  (eval [{symbol :symbol} {:keys [stack] :as state}]
    (update state :bindings assoc symbol (last stack)))

  PopToken
  (eval [_ {:keys [stack] :as state}]
    (update state :stack safe-pop))

  ConstantToken
  (eval [{constant :constant} state]
    (update state :stack conj constant))

  InvokeToken
  (eval [{:keys [op airity]} {:keys [stack] :as state}]
    (-> state
        (update :stack #(->> % (drop-last airity) vec))
        (update :stack conj (apply (resolve op) (take-last airity stack)))))

  IfElseToken
  (eval [{:keys [true-clause false-clause]} {:keys [stack] :as state'}]
    (let [state (update state' :stack safe-pop)]
      (if (-> stack last boolean)
        (read-eval true-clause state)
        (read-eval false-clause state)))))

;; INTERFACE

(defmacro defstackfn [name vars & clauses]
  `(defn ~name [& args#]
     (let [var-list# (quote ~vars)]
       (assert (= (count var-list#) (count args#)) (str "Wrong number of args, expecting " (count var-list#)))
       (-> (read-eval '(~@clauses) (assoc empty-state :bindings (zipmap var-list# args#)))
           :stack
           last))))

(comment

  (defstackfn x
    [!a !b !c]
    !a
    !b
    (invoke> + 2)
    !v1+
    !c
    !c
    <pop>
    2
    (invoke> * 2)
    !v2+
    (invoke> = 2)
    (if>
      !v1
      !v2
      (invoke> - 2)
      else>
      "false!!!"
      (invoke> println 1)
      <pop>
      !v1
      !v2
      (invoke> * 2)))

  (x 1 2 4) ;; => prints false!!!, returns 24

  )
