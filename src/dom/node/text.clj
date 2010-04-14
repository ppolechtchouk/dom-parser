(ns dom.node.text
    #^{:author "Pavel Polechtchouk"
     :doc "Converts DOM classes to string representations"}
   (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	   [dom.node.node]))


(defn trim-whitespace
  "Returns a string with excessive whitespace removed"
  [#^String s]
  (str-join "\n" 
	    (map #(.trim %) (re-split (re-pattern "\n") s))))

(defn remove-empty-lines
  "Returns a string with trimmed whitespace and empty lines removed"
  [#^String s]
  (str-join "\n" 
	    (filter #(not (empty? %)) (map #(.trim %) (re-split (re-pattern "\n") s)))))


(defn normalize-text!
  "Removes all text nodes that contain only whitespace. If there are any element keywords in the ignore_elements, the children of these elements will not be touched. 
For example (normalize! node :pre) will not touch the child text nodes of the <pre> element"
  [node & ignore_elements]
  (let [ignore? (if ignore_elements
		  (fn [n] (element? (get-parent n) (set ignore_elements)))
		  (fn [_] false))]
    )) ; TODO
