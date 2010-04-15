(ns dom.node.text
    #^{:author "Pavel Polechtchouk"
     :doc "Package for managing the text nodes. Note that for most operations, you should normalize the text nodes using normalize-text! function"}
   (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	   [dom.node.node]
	   [dom.node.element]))


(defn blank?
  "Returns true all characters in a string are whitespace"
  [s]
  (every? #(Character/isWhitespace %) s))

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

(defn replace-text!
  "Replaces the text node by another node that contains the new text. Returns the old text node if successful, or nil"
  [node text]
  (when (text-node? node)
    (replace-node! node (.createTextNode (get-document node) text))))

(defn get-text
  "Returns the contents of this TEXT node or nil if not text"
  [node]
  (when (text-node? node)
    (get-node-value node)))

(defn blank-text?
"Returns true if the TEXT node contains only whitespace or an empty string"
  [node]
  (and (text-node? node) (blank? (get-node-value node))))

(defn normalize-text-fn!
  "Similar to normalize-text! , only instead of elements to ignore you need to supply a function that will return true for nodes that should normalized"
  [node ignore_fn?]
  (cond
   (nil? node) nil
   (ignore_fn? node) node
   (blank-text? node) (do (remove-node! node) nil)
   (text-node? node) (replace-text! node (remove-empty-lines (get-text node)))
   :default (doseq [n (get-children node)] (normalize-text-fn! n ignore_fn?))))

(defn normalize-text!
  "Removes all text nodes that contain only whitespace and trims the other text nodes. If there are any element keywords in the ignore_elements, the children of these elements will not be touched. 
For example (normalize! node :pre) will not touch the child text nodes of the <pre> element"
  [node & ignore_elements]
  (let [ignore_fn? (if ignore_elements
		  (fn [x] (element? x (set ignore_elements)))
		  (fn [_] nil))]
  (normalize-text-fn! node ignore_fn?)))
