(ns tml.inspector.variables
   #^{:author "Pavel Polechtchouk"
     :doc "A package for managing TML variable operations. 
This includes:
* converting from DOM nodes to variable maps and back
* creating new variables
* creating variable maps"}
   (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	   [dom.node.node]
	   [dom.node.element]
	   [dom.node.comment]
	   [dom.parser]))

(def #^{:doc "Default Document object used to build variable nodes"} 
     *document* 
     (. (get-document-builder) (newDocument)))

(defstruct #^{:doc "General TML variable structure"} 
    variable :name :type :permissions :value :format :system :source :comment :block-comment :volatile)


(defn default-var-value
  "Returns the default variable value (as string) depending on the TML variable type. If the type is not one of string, integer, date or opaque, returns ni"
  [type]
  (cond (= type "integer") "0"
	(= type "opaque") ""
	(= type "date") "1970/01/01"
	(= type "string") ""
	:default nil))

(defn default-var-format
  "Returns the default variable format (as string) depending on the TML variable type. If the type is not one of string, integer, date or opaque, returns nil"
  [type]
   (cond (= type "integer") "-0*"
	 (= type "opaque") "base64"
	 (= type "date") "YYYY/MM/DD"
	 (= type "string") "c*"
	 :default nil))

(defn dom-node-to-var
  "Turns a vardcl element node to a variable map. The function will fill the default parameter values. If source is not supplied, it will be set to nil. If system parameter is not nil, the variable is in the system scope.
Returns a map or nil if error."
  ([node]
     (dom-node-to-var node nil))
  ([node source & system]
     (when (element? node :vardcl)
       (let [atts (get-attributes node)
	     type (or (:type atts) "string")]
	 (struct-map variable
	   :name (:name atts)
	   :type type
	   :permissions (or (:perms atts) "rw-rw")
	   :value (or (:value atts)
		      (default-var-value type))
	   :format (or (:format atts)
		      (default-var-format type))
	   :volatile (or (:volatile atts) "yes")
	   :comment (get-comment node)
	   :block-comment (get-block-comment node)
	   :system (if system true)
	   :source source)))))

(defn get-declared-var-list
"Parses all vardcl elements that are the children of this node. So, for a TML file, the node should be a <tml> element.
Returns a list of variable maps"
([node]
   (get-declared-var-list node nil))
 ( [node source & system]
    (map #(dom-node-to-var % source system)
     (filter #(element? % :vardcl) (get-children node)))))


(defn split-when
  "Separates the coll into a vector of sub-vectors. Each sub-starts starts with the item for which (pred item) returns true. pred must be free of side effects."
  [pred coll]
  (loop [result [], current [(first coll)], items (next coll)]
    (cond
     (nil? items) 
       (if (empty? current) 
	 result 
	 (conj result current)) ; return nil if empty?
     (pred (first items)) 
       (recur 
	(conj result current)
	(vector (first items))
	(next items))
     :default
       (recur
	result
	(conj current (first items))
	(next items)))))

;; TODO
;; 
;; var-to-dom-node
;; auto processing of embedded, and varlib configs
;; 

