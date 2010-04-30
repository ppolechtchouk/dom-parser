(ns tml.inspector.docs
 #^{:author "Pavel Polechtchouk"
     :doc "A package for creating docs for TML variables, pages and screens. "}
   (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	   [clojure.contrib.duck-streams :only (spit)]
	   [dom.node.node]
	   [dom.node.element]
	   [dom.node.comment]
	   [dom.node.text :only (normalize-text!)]
	   [dom.parser]
	   [tml.inspector.variables]))

;; VARDCL reporting functions

(defn var-to-html
  "Returns a string that is an html representation of a single variable map as table rows. So, they need to be wrapped into table tags."
  [m]
  (str "<tr style=\"var_params\"><td style=\"var_name\">" (:name m) 
       "</td><td style=\"var_type\">" (:type m) 
       "</td><td style=\"var_perms\">" (:permissions m) "</td></tr>\n"
       "<tr><td style=\"var_comment\" span=\"3\">" (:comment m) "</td></tr>\n"))

(defn var-coll-to-html-table
  "Generates an html table from the variable map collection. The block comment of the first item is used as the table description. if accesible-only is set, only the variables that can be accessed by services will be printed."
  [coll & accessible-only]
  (let [bc (:block-comment (first coll))
	desc (if bc (str "<h3>" bc "</h3>\n"))] 
    (str desc
	"<table style=\"var_table\">\n"
	(apply str 
	       (map var-to-html 
		    (if accessible-only
		      (filter service-accessible? coll)
		      coll)))
	"</table>\n")))

(defn var-coll-to-html-tables
  "Generates an html page from the variable map collection. The collection is split into tables based on the block comments.
The block comments are used as the titles of each table. if accesible-only is set, only the variables that can be accessed by services will be printed."
  [coll & accessible-only]
  (apply str 
	 (map (if accessible-only
		  #(var-coll-to-html % true)
		  #(var-coll-to-html %))
	      (split-when :block-comment coll))))


(defn var-generate-html-docs
  "Saves a vars.htm file that contains the doc in the output-dir"
  [output-dir & sources]
  (spit (new java.io.File output-dir "vars.htm") 
   (str "<html>\n"
	(apply str (map #(do (normalize-text! % :pre) 
			     (var-coll-to-html-tables (get-vardcl-list %)))(parse-all sources)))
	"</html>\n")))



