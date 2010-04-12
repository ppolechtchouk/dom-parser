(ns dom.printer
  #^{:author "Pavel Polechtchouk"
     :doc "Converts DOM classes to string representations"}
  (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	  [dom.parser])
  (:import  [javax.xml.parsers DocumentBuilderFactory DocumentBuilder]
	    [org.w3c.dom Document Node NodeList]
	    [java.net URL]
	    [org.xml.sax InputSource]
	    [java.io StringReader]))


(def *tab* "  ") ;element tabulation 
(def *encoding* "ISO-8859-1") ;xml document encoding

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

(defn str-attributes
  "Returns a string representation of the attributes of the node, or nil if there are none"
  [#^Node node]
  (when-let [atts (node-list (.getAttributes node))]
    (apply str (map 
		#(str " " (.getNodeName %) "=\"" (.getNodeValue %) "\"") 
		atts))))

(defmulti str-node
   "Returns a string representation of the node object with the correct tabulation. At each iteration the node string is preceded by an extra *tab*"
   (fn [node tab] (.getNodeType node) ))

(defmulti str-element
  "Returns a string representation of an Element node (and its children)"
  (fn [node tab] (.getNodeName node)));restrict to elements only??

(defn to-str 
  "Returns a string representation of the Node. In fact, it is simply a convenience function - it simply calls str-node with initial tab value"
  [node]
  (str-node node ""))

(defmethod str-node Node/DOCUMENT_NODE
  [node tab]
  (str tab "<?xml version=\"1.0\" encoding=\"" *encoding* "\"?>"
       (str-node (.getDocumentElement node) tab)))

(defmethod str-node Node/DOCUMENT_FRAGMENT_NODE
  [node tab]
  (apply str
	 (map #(str-node % tab)
	      (get-children node))))

(defmethod str-node Node/COMMENT_NODE 
  [node tab]
  (str "\n\n" tab "<!--" (.getNodeValue node) "-->"))

(defmethod str-node Node/TEXT_NODE 
  [node tab] 
  (let [text (remove-empty-lines (.getNodeValue node))]
    (if (empty? text)
      nil
      (str "\n" tab text))))

(defmethod str-node Node/ENTITY_REFERENCE_NODE 
  [node tab]
  (str "&" (.getNodeValue node) ";"))

(defmethod str-node Node/ELEMENT_NODE
  [node tab]
  (str-element node tab))

(defmethod str-element :default
  [node tab]
   (str "\n" tab "<" (.getNodeName node) (str-attributes node) 
       (if (.hasChildNodes node)
	 (str ">"
	      (apply str 
		     (map #(str-node % (str tab *tab*))
			  (get-children node)))
	      "\n" tab "</" (.getNodeName node) ">")
	 "/>")))

(defmethod str-element "screen"
  [node tab]
  (str
    (if (= Node/COMMENT_NODE 
	   (.. node (getPreviousSibling) (getNodeType)))
      nil
      "\n")
    "\n" tab "<" (.getNodeName node) (str-attributes node) 
    (if (.hasChildNodes node)
      (str ">"
	   (apply str 
		  (map #(str-node % (str tab *tab*))
		       (get-children node)))
	   "\n" tab "</" (.getNodeName node) ">")
      "/>")))



