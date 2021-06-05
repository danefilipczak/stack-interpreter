(ns stack-interpreter.core
  (:require [clojure.string :as s]))

;; TOKENS
;; Valid statements in the language

(defrecord VariableToken [symbol])

(defrecord VariableDeclarationToken [symbol])

(defrecord PopToken [])

(defrecord ConstantToken [constant])

(defrecord InvokeToken [op airity])

(defrecord IfElseToken [true-clause false-clause])

;; READERS
;; Read data and return a valid token, or throw if invalid

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
  (read [symbol]
    (cond
      (= symbol '<pop>) (PopToken.)
      (variable-declaration? symbol) (VariableDeclarationToken. symbol)
      (variable? symbol) (VariableToken. symbol)
      :else (throw (ex-info "Tried to parse invalid symbol" {:symbol symbol})))))

(defn invocation? [list]
  (and
    (= (count list) 3)
    (= (first list) 'invoke>)
    (ifn? (second list))
    (nat-int? (last list))))

(extend-protocol IRead
  Object
  (read [constant]
    (ConstantToken. constant)))

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

(defprotocol IEval
  (eval [token state]))

(def empty-state {:bindings {}
                  :stack []})

(extend-protocol IEval
  VariableToken
  (eval [{symbol :symbol} {:keys [bindings stack] :as state}]
    (if-let [value (get bindings symbol)]
      (update state :stack conj value)
      (throw (ex-info "Use of undeclared Var" {:var symbol})))))

(comment

  (eval (VariableToken. 69) {:bindings {69 10} :stack []})



  (map ["read"])

  (map read
       ['!a
        '!b
        '(invoke> + 2)
        '!v+
        '!c
        '<pop>
        2
        '(invoke> * 2)
        '!v2+
        '(invoke> = 2)])

  (invocation? (list 'invoke> + 3))

  (read (list 'invoe> #(+ 2 3) 3))

  (= 'invoke> (first (list 'invoke> + 2)))

  (= 'invoke> 'invoke>)
  (read ['hi 'dane ()])

  (variable? '!a)

  (-> '!+ symbol name count)

  (clojure.string/starts-with? (name '!a) "!")

  (clojure.string/ends-with? (name '!a+) "+")


  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
