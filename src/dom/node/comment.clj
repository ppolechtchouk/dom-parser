(ns dom.node.comment
    #^{:author "Pavel Polechtchouk"
     :doc "Package for managing comment nodes. For most oerations, the text nodes using dom.node.text/normalize-text! function.
There are two types of comments:
<!-- block comment -->
<!-- comment -->
<element>
In other word an element's comment should be immediately above the element, while a block comment that applies to several elements should be immediately above another comment."}
   (:use   [clojure.contrib.str-utils :only (re-split str-join)]
	   [dom.node.node]
	   [dom.node.element]))
(defn normalize-comments! 
  "If there are more than 2 comments in a row, the top comments are collapsed into one.  Note that for this function to work properly , you should first normalize the DOM structure using dom.node.text/normalize-text! function"
  [node]
  nil);TODO

(defn has-comment?
  "Returns true if the node has a comment immediatly above the node"
  [node]
  (comment-node? (previous-sibling node)))

(defn has-block-comment?
  "Returns true if there is a block comment (a comment above a comment) immediately above the current node"
  [node]
  (and (has-comment? node)
       (has-comment? (previous-sibling node))))

(defn get-comment
  "Returns a comment text immediately above the node, or nil if none.  Note that for this function to work properly with the elements, you should normalize the DOM structure using dom.node.text/normalize-text! function"
  [node]
  (if (has-comment? node)
    (get-node-value (previous-sibling node))))

(defn get-block-comment
  "Returns the block comment text of the current node or nil if none"
  [node]
  (if (has-block-comment? node)
    (get-node-value (previous-sibling node))))

(defn insert-comment!
  "Insert a comment node immediately above the node. This function does not do any checks, it is better to use update-comment! or update-block-comment! instead. Returns the new comment node."
  [node text]
  (insert-before! node (.createComment (get-document node) text)))

(defn merge-comments!
  "Creates a new comment node from the node (if it is a comment) and the comment node immediately above it and replaces them. The new node is returned. If the node is not a comment, or there is no comment immediately above, the function does nothing and returns nil"
  [node]
  (when (and (comment-node? node) (has-comment? node))
    ));;TODO

(defn update-comment!
  ""
  [node text]); TODO

(defn update-block-comment! 
  ""
  [node text]); TODO

;; TODO
;; block-comment?