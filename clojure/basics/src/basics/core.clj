(ns basics.core
  (:gen-class))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main [& args]
  ;list
  '(1 2 3)
  (prn (list 1 2 3))

  ; vector
  (prn '[1 2 3])

  ; map
  (prn '{:a 1 :b 2 :c 3})
  ;{1 "a" 2 "b"}

  ; set
  (prn '#{1 2 3}))

