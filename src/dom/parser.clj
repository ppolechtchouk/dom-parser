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
  (. (get-document-builder) (parse (new java.io.FileInputStream f))))

(defn parse-uri
  "Parses the tml represented by the URI and returns a Document instance"
  [#^String uri]
 (. (get-document-builder) (parse uri)))

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

; TODO error handling


