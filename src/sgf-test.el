;; A test version that represents sgf-tree as a my-tree object 

(require 'cl)
(require 'my-tree)
;; an SGF recursive descent parser
;; a SGF tree is a my-tree object whose leaves are SGF nodes
;; a SGF node is a list of SGF properties

;; This is the top level function
(defun sgf-collection ()
  "Parse a collection of sgf game trees at point.Leave the point to the end of the collection."
  (sgf-one-or-more 'sgf-gametree))

;; This can also be considered as top level;it parses one game tree.
(defun sgf-gametree ()
  (let (node-list tree-list)
    (when (and (sgf-lpar)
	       (or (setq node-list (sgf-one-or-more 'sgf-node)) (sgf-parse-error "at least one node should be present"))
	       (setq tree-list (sgf-zero-or-more 'sgf-gametree))
	       (or (sgf-rpar) (sgf-parse-error "no right paren")))
      (list node-list tree-list))))

(defun sgf-node ()
  (and
   (sgf-char ?\; )
   (sgf-zero-or-more 'sgf-property)))

(defun sgf-property ()
  (let (id val)
    (when (and
	   (setq id (sgf-propident))
	   (or (setq val (sgf-one-or-more 'sgf-propvalue)) (sgf-parse-error "no property value")))
      (cons id val)))) 
   
(defun sgf-propident ()
  (sgf-space)
  (when (looking-at "[A-Z]+")
    (goto-char (match-end 0))
    (match-string 0)))
(defun sgf-propvalue ()
  (sgf-space)
  (let (val)
    (when (and (sgf-char ?\[)
	       (or (setq val (sgf-valuetype))  (sgf-parse-error "invalid property value" ))
	       (or (sgf-char ?\]) (sgf-parse-error "no ending ']' in property value")))
      val)))

(defun sgf-valuetype ()
  (sgf-space)
  (looking-at "\\(\\\\.\\|[^]]\\)*")
  (goto-char (match-end 0))
  (match-string 0))

(defsubst sgf-space ()
  (when (looking-at "[ \t\n\v\r\f]+")
      (goto-char (match-end 0))
      (match-string 0)))
  
(defsubst sgf-char (char)
  (sgf-space)
  (when (eq (char-after) char)
    (incf (point))
    char))

(defsubst sgf-rpar () 
  (sgf-char ?\) ))
(defsubst sgf-lpar () 
  (sgf-char ?\())

(defsubst sgf-one-or-more (a)
  (loop for x = (funcall a) while x
	collect x))

(defsubst sgf-zero-or-more (a)
  (or (sgf-one-or-more a) t))

(defun sgf-parse-error (error-message &rest args)
  (apply 'error
	 (concat "sgf parse error:" error-message " at line %d column %d") 
	 (append args (list (count-lines (point-min) (point)) (current-column)))))
;; END parser def



(defun sgf-num-to-alpha (num)
  (unless (and (> num 0) (< num 27))
    (error (format "out of range:%s" num)))
  (+ ?a  num -1) )
(defun sgf-alpha-to-num (a)
  (unless (and (>= a  ?a) (<= a ?z))
    (error (format "out of range:%s" a)))
  (+ 1 (- a  ?a)))
(defun sgf-coordinate (val)
  (assert (= (length val) 2))
  (list (sgf-alpha-to-num (elt val 0)) (sgf-alpha-to-num (elt val 1))))

(defun sgf-point (x y)
  (string (sgf-num-to-alpha x) (sgf-num-to-alpha y)))
(defun sgf-column (val)
  (first (sgf-coordinate val)))
(defun sgf-row (val)
  (second (sgf-coordinate val)))


;; following functions convert sgf data to my-tree object 	    

(defstruct (sgf-property
	    (:constructor nil)
	    (:constructor make-sgf-property (id value-list))
	    )
  id value-list)
(defun sgf-property-add-value (prop val)
  (setf (sgf-property-value-list prop)
	(nconc (sgf-property-value-list prop) (list val))))

(defun sgf-property-delete-value (prop val)
  (setf (sgf-property-value-list prop)
	(delete val (sgf-property-value-list prop))))
(defun sgf-property-search-value (prop val)
  (member val (sgf-property-value-list prop)))
  
(defun sgf-property-to-string (property)
  (apply 'concat 
	 (sgf-property-id property)
	 (loop for value in (sgf-property-value-list property)
	       collect (format "[%s]" value))))

(defun sgf-property-null-value-list-p (property)
  (null (sgf-property-value-list property)))

(defstruct (sgf-node
	    (:constructor nil)
	    (:constructor make-sgf-node (property-list)))
  property-list)

(defun sgf-node-add-property (node prop)
  (assert (sgf-node-p node))
  (assert (sgf-property-p prop))
  (setf (sgf-node-property-list node)
	(nconc (sgf-node-property-list node) (list prop))))

(defun sgf-node-search-property (node id &optional val)
  "Return a list of properties that belong to NODE ,have id ID , 
  and optionally have value VAL if VAL is non-nil."
  (assert (sgf-node-p node))
  (loop for prop in (sgf-node-property-list node)
	if (and (equal  id   (sgf-property-id prop))
		(or (not val) (member val  (sgf-property-value-list prop))))
	collect prop))

(defun sgf-node-add-property-value (node id val)
  (assert (sgf-node-p node))
  (let ((prop-list (sgf-node-search-property node id)))
    (if prop-list
	(sgf-property-add-value (first prop-list) val)
      (sgf-node-add-property node (make-sgf-property id (list val))))))

(defun sgf-node-delete-property (node prop)
  (assert (sgf-node-p node))
  (setf (sgf-node-property-list node) 
	(delq prop (sgf-node-property-list node))))

(defun sgf-node-canonicalize (node)
  "Remove properties whose property list is nil"
  (assert (sgf-node-p node))
  (setf (sgf-node-property-list node)
	(delete-if 'sgf-property-null-value-list-p (sgf-node-property-list node))))

(defun sgf-node-delete-property-value (node id val)
  (assert (sgf-node-p node))
  (mapcar (lambda (prop) (sgf-property-delete-value prop val))
	  (sgf-node-search-property node id val))
  (sgf-node-canonicalize node))

(defun sgf-node-value-list (node id)
  (assert (sgf-node-p node))
  (append (mapcar 'sgf-property-value-list (sgf-node-search-property node id))))

(defun sgf-node-to-string (sgf-node)
  (apply 'concat ";" (mapcar 'sgf-property-to-string (sgf-node-property-list sgf-node))))

;;sanity check
(defun sgf-property-list-p (x)
  (not (memq  nil (mapcar 'sgf-property-p x))))

(defun sgf-to-my-tree-list (sgf-tree-list)
;  (mapcar 'sgf-to-go-game sgf-tree))
  (loop for sgf-tree in sgf-tree-list
	for i from 1
	collect (sgf-to-my-tree sgf-tree)))
;	collect (condition-case sig
;		    (sgf-to-my-tree sgf-tree)
;		  (error (error "error in game %d:%s" i sig )))))


(defun sgf-to-my-tree (sgf)
  (assert (= 2 (length sgf)) t)
  (let ((sgf-tree (make-sgf-tree))
	(node-list (first sgf))
	(tree-list (second sgf)))
    (sgf-to-my-tree-handle-node-list node-list sgf-tree)
    (unless (eq t tree-list)
      (sgf-to-my-tree-handle-tree-list tree-list sgf-tree))
    sgf-tree))

(defun sgf-to-my-tree-handle-tree-list (tree-list sgf-tree)
  (assert (consp tree-list) t)
  (loop for tree in tree-list
	do (sgf-to-my-tree-handle-tree tree sgf-tree)))

(defun sgf-to-my-tree-handle-tree (tree sgf-tree)
  (assert (= 2 (length tree)) t)
  (let ((node-list (first tree))
	(tree-list (second tree))
	(current-node (my-tree-top sgf-tree)))
    (assert (consp node-list) t)
    (sgf-to-my-tree-handle-node-list node-list sgf-tree)
    (if  (and tree-list (not (eq t tree-list)))
	(sgf-to-my-tree-handle-tree-list tree-list sgf-tree))
    (my-tree-pop-to-node sgf-tree current-node))
  sgf-tree)
    

(defun sgf-to-my-tree-handle-node-list (node-list sgf-tree)
  (assert (consp node-list) t)
  (loop for node in node-list
	do (sgf-to-my-tree-handle-node node sgf-tree)))


(defun sgf-to-my-tree-handle-node (node sgf-tree)
  (assert (consp node) t)
  (let ((new-node
	 (make-sgf-node
	  (loop for property in node
		collect (make-sgf-property (first property) (rest property))
		))))
    (my-tree-add-child sgf-tree new-node)))




;; methods to manipulate sgf-tree object
(defun make-sgf-tree ()
  (make-my-tree 'sgf-node-p 'sgf-node-to-string))
(defun sgf-tree-search-child (tree id &optional val)
  (first (member-if (lambda (node)
		      (sgf-node-search-property node id val))
		    (my-tree-child-node-list tree))))

(provide 'sgf-test)
