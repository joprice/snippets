(ns basics.core
  (:gen-class))

; similar to import static - reference code in other namespace as if it is local
(use 'clojure.set)
(require 'clojure.string)

(defn lists
  "lists"
  []
  (prn "lists")
  (prn (list 1 2 3))
  ; or quoted
  '(1 2 3)
  (prn (first '(1 2 3)))
  (prn (rest '(1 2 3)))
  ; get first/rest support from other various structures
  (seq #{1 2 3 4 5}))

(defn maps
  "maps"
  []
  (prn "maps")
  ; map literals are hashmaps
  (let [a {:a 1 :b 2}]
    (prn a)
    ; map and symbols are functions
    (prn (:a a))
    (prn (a :a))
    ; join data structures with merging function
    (prn (merge-with + a {:c 3 :d 4}))
    (prn (keys a))
  ))

(defn vectors
  "vectors"
  []
  (prn "vectors")
  ; random access
  (prn [1 2 3])
  (prn ([1 2 3] 2)))

(defn sets
  "sets"
  []
  (prn "sets")
  ; hash literals are hashsets
  (prn (union #{1 2 3} #{3 4 5}))
  (prn #{1 2 3}))

(defn overloaded
  ([a] (overloaded a 2))
  ([a b] (prn '(a b))))

(defn -main [& args]
  (lists)
  (maps)
  (vectors)
  (sets)

  (overloaded 1)
  (overloaded 1 2)

  ; var that is meant to be rebound - the name is convention and
  ; dynamic makes this explicit
  (def ^:dynamic *rebindable* "initial")

  ; without dynamic, optimizations are made since it will not be rebound
  (def static "static")

  ; regex literal
  #","

  'foo ;foo
  `foo ;user/foo
  (def foo 5) ;#'user/foo
  (resolve 'foo) ; #'user/foo

  ; destructuring
  (let [[a b c] [1 2 3]] (prn b))

  ; use or as a fallback if match fails
  (let [{a :a, b :b, :as m :or {a 5 b 7}} {:a 1 :b 2}] (prn b))

  ; build map from vectors
  (into {} (map vector [:a :b :c] [1 2 3]))

  ; anonymous function - ignore parameters with _
  ((fn [x _] x) 2 3)

  (.toLowerCase "AHHHHH")

  ; reference equality
  (identical? 'a 'a)
  ; value equality
  (= 'a 'a)

  ; null
  nil
  ; multimethods
  (defmulti encounter (fn [x y] [(:Species x) (:Species y)]))

  (defmethod encounter [:Bunny :Lion] [b l] :run-away)
  (defmethod encounter [:Lion :Bunny] [l b] :eat)

  (def b {:Species :Bunny})
  (def l {:Species :Lion})
  
  (prn (encounter b l))
  (prn (encounter l b))

  ; for loop returns another collection
  ; doseq is used for procedures returning nil
  (doseq [x (for [i (range 10)] (+ i 10))]
    (prn x))
    
  ; metadata
  (def v [1 2 3])
  (def trusted (with-meta v {:source :trusted}))

  ;;; STM - uses Multiversion Concurrency Control

  ;; refs
  ; - shared, synchronous, transactional, atomic
  (def r (ref {:a 1 :b 2 :c 3}))
  ; deref
  @r
  ; read and modify ref - next dereference will have new value
  (dosync (commute r assoc :c 5))

  ;; agents - shared, asynchronous, autonomoous 
  ; - can do side effects
  ; dispatches made during an action are held until after the 
  ; state of the agent has changed
  (def a (agent {:a 1 :b 2 :c 3}))
  @a
  ; update change asynchronously
  (send a assoc :d 4)
  ; wait for change
  (await a)

  ; type hints
  (defn len [^String x] (.length x))

  (let [a [1 2 3]
        b {:a 1 :b 2}
        c (list 1 2 3)]
    ;same interface for all collections
    (prn [(conj a 4)
          (conj b [:c 3])
          (conj c 4)
          a b c])))





