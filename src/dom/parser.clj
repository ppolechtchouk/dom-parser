(ns dom.parser
  #^{:author "Pavel Polechtchouk"
     :doc "Wrapper for generating a Document object from a TML file."}
  (:import  [javax.xml.parsers DocumentBuilderFactory DocumentBuilder]
	    [org.w3c.dom Document Node NodeList]
	    [java.net URL]
	    [org.xml.sax InputSource]
	    [java.io StringReader]))

(defn get-document-builder 
  "Returns an instance of the DocumentBuilder"
  []
  (.. (DocumentBuilderFactory/newInstance) (newDocumentBuilder)))

(defn parse-file
  "Parses a file on local disk. Returns a Document instance."
  [#^String f]
;; TODO error handling + if any problems return NULL
  (let [file (new java.io.File f)
	document (. (get-document-builder) 
		    (parse (new java.io.FileInputStream file)))]
    (.setUserData document "name" (.getName file) nil)
    (.setUserData document "path" (.getPath file) nil)
    document))

(defn parse-uri
  "Parses the tml represented by the URI and returns a Document instance"
  [#^String uri]
 (. (get-document-builder) (parse uri))) ; add source user data and error handling

(defn parse-string
  "Parses the input string and returns a Document instance."
  [s]
  (. (get-document-builder) (parse (new InputSource (new StringReader s)))))

(defn parse
  "A smart parse - uses the appropriate parse-? function based on the start of the input string."
  [#^String s]
  (let [st (.trim s)]
    (cond
      (.startsWith st "<") (parse-string st)
      (.startsWith st "http") (parse-uri st)
      :default (parse-file st))))

(defn parse-all
  "Returns a vector of document objects. Each document created from the item in the collection."
  [coll]
  (loop [result [], source (first coll), more (next coll)]
    (if source
      (recur (conj result (parse source)) (first more) (next more))
      (if (empty? result) nil result))))


; TODO error handling


