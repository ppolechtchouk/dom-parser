(ns dom.node.node
    #^{:author "Pavel Polechtchouk"
     :doc "Generic node utility and processing functions. This includes node types."}
    (:import [org.w3c.dom Document Node NodeList]))
;; Various Node types
(def
     #^{:doc "org.w3c.dom.Node/DOCUMENT_NODE equivalent"}      
     DOCUMENT   
     Node/DOCUMENT_NODE)
(def
     #^{:doc "org.w3c.dom.Node/DOCUMENT_FRAGMENT_NODE equivalent"}      
     FRAGMENT   
     Node/DOCUMENT_FRAGMENT_NODE)

(def
     #^{:doc "org.w3c.dom.Node/ELEMENT_NODE equivalent"}      
     ELEMENT   
     Node/ELEMENT_NODE)

(def
     #^{:doc "org.w3c.dom.Node/ATTRIBUTE_NODE equivalent"}      
     ATTRIBUTE   
     Node/ATTRIBUTE_NODE)

(def
     #^{:doc "org.w3c.dom.Node/COMMENT_NODE equivalent"}      
     COMMENT   
     Node/COMMENT_NODE)

(def
     #^{:doc "org.w3c.dom.Node/TEXT_NODE equivalent"}      
     TEXT   
     Node/TEXT_NODE)
(def
     #^{:doc "org.w3c.dom.Node/ENTITY_NODE equivalent"}      
     ENTITY   
     Node/ENTITY_NODE)

(def
     #^{:doc "org.w3c.dom.Node/CDATA_SECTION_NODE equivalent"}      
     CDATA   
     Node/CDATA_SECTION_NODE)


(defn node-list
  "Returns a list of nodes that are contained in the NodeList or NamedNodeMap. If there are no nodes, returns nil."
  [nl]
  (when nl
    (loop [result nil index (dec (.getLength nl))]
      (if (>= index 0)
	(recur (conj result (.item nl index)) (dec index))
	result))))

(defn get-children
  "Returns a list of child nodes or nil if the node has no children."
  [#^Node node]
  (node-list (.getChildNodes node)))

(defn filter-out-children
  "Returns a list of child nodes with nodes of the selected types removed. Only immediate children are affected - i.e. this function purges only 1 level deep."
  [node & types]
   (let [rs (set types)]
     (filter #(not (contains? rs (.getNodeType %))) (get-children node))))

(defn node?
  "Returns true if the object implements the Node interface."
  [o]
  (instance? Node o))

(defn element-node? 
  "Returns true if the node is of the ELEMENT type."
  [node]
  (try
   (and (node? node) (= ELEMENT (.getNodeType node)))
   (catch Exception _ false)))

(defn comment-node? 
  "Returns true if the node is of the COMMENT type."
  [node]
  (try
   (and (node? node) (= COMMENT (.getNodeType node)))
   (catch Exception _ false)))

(defn document-node? 
  "Returns true if the node is of the DOCUMENT type."
  [node]
   (instance? Document node))


(defn get-root
  "Returns the root node of the document. Input can be any node."
  [node]
  (if (document-node? node)
    (.getDocumentElement node)
    (.. node getOwnerDocument getDocumentElement)))

(defn get-document
  "Returns the Document the node belongs to, or nil if none"
  [node]
  (if (node? node)
    (.getOwnerDocument node)))

(defn purge-children!
  "Removes all the child nodes of the supplied node type and returns the node. Note that this is a destructive operation! Can be used to clean up the DOM.
Usage: (purge-children! node TEXT ENTITY) - removes all child nodes that are of the TEXT or ENTITY type"
  [node & TYPES]
  (let [t (set TYPES)]
    (doseq [cn (filter #(contains? t (.getNodeType %)) (get-children node))] 
     (.removeChild node cn))
    node))

(defn next-sibling
  "Returns the next sibling of the node or nil if none or error"
  [node]
  (try
   (.getNextSibling node)
   (catch Exception _ nil)))

(defn previous-sibling
  "Returns the previous sibling of the node or nil if none or error"
  [node]
  (try
   (.getPreviousSibling node)
   (catch Exception _ nil)))

(defn get-parent
  "Returns a parent node or nil if none or error"
  [node]
  (try
   (.getParentNode node)
   (catch Exception _ nil)))

(defn insert-before!
  "Inserts new_node directly above the node and returns the new_node if everything is ok or nil.
If new_node is a document fragment node the contents of the document fragment are inserted in order."
  [node new_node]
  (try
   (.insertBefore (get-parent node) new_node node)
   (catch Exception _ nil)));;TODO check what should be returned

(defn take-nodes-while
  "Calls get_fn on the start_node and then repeats on the return of get_fn. This continues while get_fn returns a node and check_fn returns true. All functions recovered this way are returned as a vector. start_node is included but needs to pass check_fn as well. If no nodes are suitable, returns nil.
If check_fn is not supplied, all nodes are assumed to pass the test.
get_fn and check_fn should take a node as the parameter and should be free of side effects.
Usage:
 (take-nodes-while node next-sibling element?) will return a vector of continuous element nodes.
 (take-nodes-while node previous-sibling element?) will do the same thing but going up."
([start_node get_fn]
   (take-nodes-while start_node get_fn (fn [_] true)))
([start_node get_fn check_fn]
   (loop [result [] 
	  node start_node]
     (if (and (node? node) (check_fn node))
       (recur (conj result node) (get_fn node))
       (if (empty? result) nil result)))))


;; TODO
;; first-child , last-child 
;; replace-node! , delete-node!

