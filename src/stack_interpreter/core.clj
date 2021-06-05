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

(defn safe-pop [stack]
  (cond-> stack
    (not-empty stack)
    pop))

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
        (update :stack conj (apply op (take-last airity stack))))))

(defmacro spy [expr]
  `(do (println ~expr "\uD83D\uDC40 ")
       ~expr))

(defn read-eval [clause state]
  (->> clause
       (map read)
       (reduce
         (fn [prev-state token]
           (eval token prev-state))
         state)))

(extend-protocol IEval
  IfElseToken
  (eval [{:keys [true-clause false-clause]} {:keys [stack] :as state'}]
    (let [state (update state' :stack safe-pop)]
      (if (-> stack last boolean)
        (read-eval true-clause state)
        (read-eval false-clause state)))))


(def empty-state {:bindings {}
                  :stack []})

(comment


  (read-eval ['!a
              '!b
              (list 'invoke> + 2)
              '!v1+
              '!c
              '!c
              '<pop>
              2
              (list 'invoke> * 2)
              '!v2+
              (list 'invoke> = 2)
              (list 'if>
                    '!v1
                    '!v2
                    (list 'invoke> - 2)
                    'else>
                    "false!!!"
                    (list 'invoke> println 1)
                    '<pop>
                    '!v1
                    '!v2
                    (list 'invoke> * 2))]
             (update empty-state :bindings assoc '!a 1 '!b 2 '!c 4))

  (eval (InvokeToken. 2) {:bindings {69 10} :stack [1 2]})

  (map ["read"])

  (read-eval ['!a
              '!b
              '(invoke> + 2)
              ;'!0+
              ;'!c
              ;'<pop>
              ;2
              ;'(invoke> * 2)
              ;'!v2+
              ;'(invoke> = 2)
              ]
             (update empty-state :bindings assoc '!a 1 '!b 2 '!c 4))

  (map read
       ['!a
        '!b
        '(invoke> + 2)
        '!0+
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
