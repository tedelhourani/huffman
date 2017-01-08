(ns huffman.core
  (:gen-class)
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn make-leaf
  "I make a leaf"
  [symbol weight]
  (list :leaf symbol weight))

(defn leaf?
  "I check if my argument is a leaf"
  [object]
  (= (first object) :leaf))

(defn symbol-leaf
  "I get the symbol of this leaf"
  [leaf]
  (first (rest leaf)))

(defn weight
  "I get the weight of the leaf"
  [leaf]
  (last leaf))

(defn symbols
  "I get tree symbol list or leaf symbol"
  [object]
  (if (leaf? object)
    (list (symbol-leaf object))
    (second (reverse object))))
    
(defn make-code-tree
  "I make a tree out of left and right branches"
  [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn insert-node
  ""
  [tree node]
  (concat (filter #(<= (weight %) (weight node)) tree)
          (list node)
          (filter #(> (weight %) (weight node)) tree)))

(defn successive-merge
  ""
  [tree]
  (if (= (count tree) 1) tree
      (successive-merge
       (insert-node 
        (rest (rest tree))
        (make-code-tree (first tree) (second tree))))))

(defn make-huffman-tree
  "I make the Huffman code"
  [& symbol-weight]
  (let [sorted-by-value (apply priority-map symbol-weight)]
    (successive-merge (map make-leaf
                  (keys sorted-by-value)
                  (vals sorted-by-value)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (make-huffman-tree :B 3 :A 8 :C 1 :F 1 :E 1 :D 1 :G 1 :H 1)))
