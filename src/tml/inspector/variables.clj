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
	   [dom.node.comment]))

(def #^{:doc "Default Document object used to build variable nodes"} 
     *document* 
     (. (get-document-builder) (newDocument)))

(defstruct #^{:doc "General TML variable structure"} 
    variable :name :type :permissions :value :format :system :source :comment :block-comment :volatile)

(defn dom-node-to-var
  "Turns a vardcl element node to a variable map. The function will fill the default parameter values. If source is not supplied, it will be set to nil. If system parameter is not nil, the variable is in system scope.
Returns a map or nil if error."
  ([node]
     (dom-node-to-var node nil))
  ([node source & system]
     (when (element? node :vardcl)
       (let [atts (get-attributes node) type (:type atts)]
	 (struct-map variable
	   :name (:name atts)
	   :type (or type "string")
	   :permissions (or (:perms atts) "rw-rw")
	   :value (:value atts)
	   :format (or (:format atts)
		       (cond (= type "integer") "-0*"
			     (= type "opaque") "base64"
			     (= type "date") "YYYY/MM/DD"
			     :default "c*")) ;string
	   :volatile (or (:volatile atts) "yes")
	   :comment (get-comment node)
	   :block-comment (get-block-comment node)
	   :system (if system true)
	   :source source)))))

;; TODO
;; 
;; var-to-dom-node
;; get-declared-variables-list
;; 

