(ns dom.parser
  #^{:author "Pavel Polechtchouk"
     :doc "Wrapper for generating a Document object from a TML file."}
  (:import  [javax.xml.parsers DocumentBuilderFactory DocumentBuilder]
	    [org.w3c.dom Document Node NodeList]
	    [java.net URL]
	    [org.xml.sax InputSource]
	    [java.io StringReader]))
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

(defn get-document-builder 
  "Returns an instance of the DocumentBuilder"
  []
  (.. (DocumentBuilderFactory/newInstance) (newDocumentBuilder)))

(defn parse-file
  "Parses a file on local disk. Returns a Document instance."
  [#^String f]
;; TODO error handling + if any problems return NULL
  (. (get-document-builder) (parse (new java.io.FileInputStream f))))

(defn parse-uri
  "Parses the tml represented by the URI and returns a Document instance"
  [#^String uri]
 (. (get-document-builder) (parse uri)))

(defn parse-string
  "Parses the input string and returns a Document instance."
  [s]
  (. (get-document-builder) (parse (new InputSource (new StringReader s)))))

(defn sparse
  "A smart parse - uses the appropriate parse-? function based on the start of the input string."
  [#^String s]
  (let [st (.trim s)]
    (cond
      (.startsWith st "<") (parse-string st)
      (.startsWith st "http") (parse-uri st)
      :default (parse-file st))))

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

(defn element?
  "Returns true if the node is one of the elements in the set. If no elements are supplied, returns true if the node is of the ELEMENT type.
Usage example:
(element? node) 
(element? node :setvar :vardcl)"
  ([node] (element-node? node))
  ([node & elements]
     (and (element-node? node)
	  (contains? (set elements) (to-keyword (.getNodeName node))))))

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

(defn get-comment
  "Returns a COMMENT node directly above the node, or nil if none"
  [node]
  (let [pn (.getPreviousSibling node)]
    (if (comment-node? pn)
      pn
      nil)))

(defn insert-before
  "Inserts new_node directly above the node and returns the new_node if everything is ok or nil "
  [node new_node]
  );;TODO exception handling
(defn add-comment
  "Adds a COMMENT node containing text directly above the node."
  [node text]
  )

(defn get-first-element-after
  "Return the first sibling of the specified element type after the node in the node list, or nil if none. element should be a keyword, i.e. :vardcl, :setvar"
  [node element]
  )

