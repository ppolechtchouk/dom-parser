(ns dom.node.element
  #^{:author "Pavel Polechtchouk"
     :doc "Element node utility and processing functions."}
  (:use dom.node.node))

(def *keywords* (agent 
		 (hash-map)
		 :meta {:doc "Hash-map of {string :string} used for keyword creation optimisation"}))

(defn update-keywords
  "Adds a new keyword to the *keywords* map, based on the value of string s. Returns a keyword based on s"
  [s]
  (let [k (keyword s)]
   (send *keywords* assoc s k)
   k))

(defn to-keyword
  "Optimised version of the keyword function that uses *keywords*. Returns an :s keyword"
  [s]
  (let [kw (get @*keywords* s)]
    (if kw
      kw
      (update-keywords s))))

(defn element?
  "Returns true if the node is one of the elements in the set. If no elements are supplied, returns true if the node is of the ELEMENT type. 
If only one parameter is given after the node, it can be either a set or a keyword. If there is more than one parameter, it is assumed that the parameters are keywords. 
The elements should be given as keywords, i.e. :pre for <pre>
Usage example:
 (element? node) 
 (element? node :setvar :vardcl)
 (element? node #{:td :th})"
  ([node] (element-node? node))
  ([node element & elements]
     (if (element-node? node)
       (cond
	elements
          (contains? (conj (set elements) element) (to-keyword (.getNodeName node)))
					; if more than one parameter, only keywords are expected
	(set? element)	  
	  (contains? element (to-keyword (.getNodeName node)))
	:default
	  (= element (to-keyword (.getNodeName node)))) ; keyword
       false)))

(defn get-attributes
  "Returns a map of :attribute \"value\" of the element node or nil if none."
  [node]
  (when (element? node)
    (apply merge (for [attr (node-list (.getAttributes node))] 
	     {(to-keyword (.getNodeName attr)) (.getNodeValue attr)}))))