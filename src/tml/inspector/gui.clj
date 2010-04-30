(ns tml.inspector.gui
  (:import [javax.swing JFrame JButton JPanel BorderFactory JTextArea JScrollPane JList DefaultListModel ListSelectionModel]
	   [java.awt.event ActionListener]
	   [java.awt BorderLayout]))

(def current-var (agent nil))


(def comment-field 
     #^{:doc "Field for displaying comment content for the currently selected item"}
 (doto (new JTextArea 10 20)
   (.setText "test")
   (.setEditable false)))

(def var-list
     #^{:doc "Field for displaying comment content for the currently selected item"}
     (doto (new JList)
       (.setLayoutOrientation JList/VERTICAL)
       (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
       (.setVisibleRowCount 10)))

(def content-pane
#^{:doc "Content pane for the gui"}
(doto (new JPanel (new BorderLayout))
  (.add 
   (doto (new JScrollPane comment-field)
     (.setBorder (BorderFactory/createTitledBorder "Comment"))))))

(def vardcl-buttons 
     (agent {}))

(defn update-var-info!
 ""
 [_ vm]
 (do (.setText comment-field (:comment vm)))
 vm)

(def action-listener 
     (proxy [ActionListener] []
       (actionPerformed 
	[e]
	(cond (= "vardcl" (.getActionCommand e)) 
	      (reset! current-var (get @vardcl-buttons (.getSource e))) ))))



(defn window
  [title]
  (doto (new JFrame title) 
    (.setSize 400 400)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setContentPane content-pane)
    (.pack)
    (.setVisible true)))

(defn vardcl-button
  [vm]
  (let [button
	(doto (new JButton (:name vm))
	  (.setActionCommand "vardcl")
	  (.setVerticalTextPosition JButton/CENTER)	 
	  (.setHorizontalTextPosition JButton/LEADING)
	  (.addActionListener action-listener))]
    (send vardcl-buttons assoc button vm)
    button))