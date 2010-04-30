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

(defn determine-source
  "Determines the variable source document based on the DOM structure and Document userdata.
Returns an array [source system-scope?] or nil if the document is not acceptable as a possible vardcl source
Source will be either:
* :embedded - embedded.tml (system scope)
* :terminal - terminal varlib (system scope)
* :service - service varlib (service scope)
* filename - the name of the source file, i.e. index.tml (service scope)"
  [node]
  (let [r (get-root node)
	n (get-document-name node)]
    (cond
     (element? r :VarlibScopeTerminalConfigData) [:terminal true]
     (element? r :VarlibScopeServiceConfigData) [:service nil]
     (and (element? r :tml) (= n "embedded.tml")) [:embedded true]
     (element? r :tml) [n nil]
     :default nil)))

(defn determine-vardcl-root
  "Returns the <vardcl> parent node based on the document type, or nil if invalid document."
  [node]
  (let [r (get-root node)]
    (cond
     (element? r :VarlibScopeTerminalConfigData :VarlibScopeServiceConfigData) (first-child (first-child r)) ; <vardcls>
     (element? r :tml) r
     :default nil)))


(defn dom-node-to-var
  "Turns a vardcl element node to a variable map. The function will fill the default parameter values. If source is not supplied, the function will attempt to resolve it and system parameter automatically as per 'determine-source' function. If system parameter is not nil, the variable is in the system scope.
Returns a map or nil if error."
  ([node]
     (let [[source system] (determine-source node)]
       (dom-node-to-var node source system)))
  ([node source system]
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

(defn can-read?
  "Returns true if the permissions allow the variable to be read by the service"
  [var]
  (if (:system var)
    (= (.charAt (:permissions var) 3) \r)
    (= (.charAt (:permissions var) 0) \r)))

(defn can-write?
  "Returns true if the permissions allow the variable to be set by the service"
  [var]
  (if (:system var)
    (= (.charAt (:permissions var) 4) \w)
    (= (.charAt (:permissions var) 1) \w)))

(defn service-accessible?
  "Returns true if the variable allows either read or write access from the service. I.e. a system var with rw--- permissions will return false"
  [var]
  (or (can-read? var) (can-write? var)))

(defn get-vardcl-list
"Parses all vardcl elements that are the children of this node. So, for a TML file, the node should be a <tml> element.
If only one parameter is given, will attempt ot resolve the <vardcl> root node, source and scope automatically, as per 'determine-source' and 'determine-vardcl-root' functions.
Returns a list of variable maps in order they were declared"
([node]
   (let [[source system] (determine-source node)]
    (get-vardcl-list (determine-vardcl-root node) source system)))
( [node source system]
    (map #(dom-node-to-var % source system)
     (filter #(element? % :vardcl) (get-children node)))))

(defn get-vardcl-map
"Parses all vardcl elements that are the children of this node. So, for a TML file, the node should be a <tml> element.
If only one parameter is given, will attempt ot resolve the <vardcl> root node, source and scope automatically, as per 'determine-source' and 'determine-vardcl-root' functions.
Returns a map of variable maps, where the keys are variable names"
([node]
   (apply merge (map #({(:name %) %}) (get-vardcl-list node))))
([node source system]
   (apply merge (map #({(:name %) %}) (get-vardcl-list node source system)))))


;; Utility functions
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
;; variable verifications

