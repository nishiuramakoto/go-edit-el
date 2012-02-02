;; A test version that represents sgf-tree as a my-tree object 

;; See elisp manual
(require 'cl)
;(eval-when-compile (require 'cl))

(require 'my-tree)

(defconst sgf-move-properties   		'("B" "KO" "MN" "W"))
(defconst sgf-setup-properties 			'("AB" "AE" "AW" "PL"))
(defconst sgf-node-annotation-properties 	'("C" "DM" "GB" "GW" "HO" "N" "UC" "V"))
(defconst sgf-move-annotation-properties 	'("BM" "DO" "IT" "TE"))
(defconst sgf-unique-markup-properties 		'("CR" "MA" "SL" "SQ" "TR"))
(defconst sgf-markup-properties 		'("AR" "CR" "DD" "LB" "LN" "LN" "MA" "SL" "SQ" "TR"))
(defconst sgf-root-properties 			'("AP" "CA" "FF" "GM" "ST" "SZ"))
(defconst sgf-game-info-properties 		'("AN" "BR" "BT" "CP" "DT" "EV" "GN" "GC" "ON" "OT" "PB" "PC" "PW" "RE" "RO" "RU" "SO" "TM" "US" "WR" "WT"))
(defconst sgf-timing-properties 		'("BL" "OB" "OW" "WL"))
(defconst sgf-misc-properties 			'("FG" "PM" "VW"))

;; An SGF recursive descent parser.
;; An SGF tree is a my-tree object whose leaves are SGF nodes.
;; An SGF node consists of a list of SGF properties.


(defun sgf-value-nth (val index)
  (nth index (split-string val ":")))


(defun sgf-opposite-color (color)
  (assert (memq color '(black white)))
  (case color
    ('black 'white)
    ('white 'black)))


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


;; sgf converter (internal use only)

(defun sgf-tree-p (tree)
  (my-tree-p tree))

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
;  (assert (consp node) t)
  (assert (or (listp node) (eq node t)))
  (when (consp node)
    (let ((new-node
	   (make-sgf-node
	    (loop for property in node
		  collect (apply 'make-sgf-property (first property) (rest property))
		  ))))
      (my-tree-add-child sgf-tree new-node))))





(defun make-sgf-tree-list-from-file (filename)
  (assert (file-readable-p filename))
  (with-temp-buffer
    (insert-file-contents filename)
;    (insert-file filename)
    (goto-char (point-min))
    (mapcar 'sgf-to-my-tree (sgf-collection))))


;; following functions convert sgf data to my-tree object 	    

(defstruct (sgf-property
	    (:constructor nil)
	    (:constructor make-sgf-property-by-list (id value-list))
	    )
  id value-list)
(defun make-sgf-property (id &rest value-list)
  (make-sgf-property-by-list id value-list))

(defun sgf-property-valid-p (prop)
  (and (sgf-property-p prop) 
       (stringp (sgf-property-id prop)) 
       (listp (sgf-property-value-list prop))))

(defun sgf-property-add-value (prop val)
  (assert (sgf-property-valid-p prop))
  (setf (sgf-property-value-list prop)
	(nconc (sgf-property-value-list prop) (list val))))

(defun sgf-property-delete-value (prop  val)
  (assert (sgf-property-valid-p prop))
  (setf (sgf-property-value-list prop)
	(delete val (sgf-property-value-list prop))))

(defun sgf-property-delete-value-if (prop pred)
  (assert (sgf-property-valid-p prop))
  (setf (sgf-property-value-list prop)
	(delete-if pred (sgf-property-value-list prop))))

(defun sgf-property-search-value (prop val)
  (assert (sgf-property-valid-p prop))
  (member val (sgf-property-value-list prop)))

(defun sgf-property-search-value-if (prop pred)
  (assert (sgf-property-valid-p prop))
  (member-if pred (sgf-property-value-list prop)))
  
(defun sgf-property-to-string (property)
  (assert (sgf-property-valid-p property))
  (apply 'concat 
	 (sgf-property-id property)
	 (loop for value in (sgf-property-value-list property)
	       collect (format "[%s]" value))))

(defun sgf-property-null-value-list-p (property)
  (null (sgf-property-value-list property)))

(defstruct (sgf-node
	    (:constructor nil)
	    (:constructor make-sgf-node (&optional property-list)))
  property-list)


;;sanity check
(defun sgf-property-list-p (x)
  (not (memq  nil (mapcar 'sgf-property-p x))))


(defun sgf-node-clone (from to)
  (setf (sgf-node-property-list to) 
	(copy-tree (sgf-node-property-list from) t)))

(defun sgf-node-add-property (node prop)
  (assert (sgf-node-p node))
  (assert (sgf-property-valid-p prop))
  (setf (sgf-node-property-list node)
	(nconc (sgf-node-property-list node) (list prop))))



(defun sgf-node-search-property (node id &optional val)
  "Return a list of properties that belong to NODE ,have id ID , 
  and  have value VAL if VAL is non-nil."
  (assert (sgf-node-p node))
  (loop for prop in (sgf-node-property-list node)
	if (and (equal  id   (sgf-property-id prop))
		(or (not val) (member val  (sgf-property-value-list prop))))
	collect prop))

(defun sgf-node-search-property-if (node pred)
  "Return a list of properties under NODE that satisfy PRED"
  (assert (sgf-node-p node))
  (remove-if-not pred (sgf-node-property-list node)))

(defun make-sgf-property-value-searcher (id value-pred)
  (lexical-let ((id id)
		(pred value-pred))
    (function  (lambda (prop)
		 (and (equal (sgf-property-id prop) id)
		      (member-if pred (sgf-property-value-list prop)))))))

(defun make-sgf-property-value-searcher-regexp (id regexp)
  (lexical-let ((regexp regexp))
    (make-sgf-property-value-searcher id (lambda (val) (string-match regexp val)))))

(defun sgf-node-search-property-value-if (node id value-pred)
  "Return a list whose car is a value that  belongs to the property ID  and matches VALUE-PRED 
or nil if no such value exists.
One can 'setcar' on this list to change the value.
"
  (some (make-sgf-property-value-searcher id value-pred) (sgf-node-property-list node)))

(defun sgf-node-search-property-value-regexp (node id regexp)
  (sgf-node-search-property-value-if node id (lambda (val) (string-match regexp val))))


(defun sgf-node-add-property-value (node id val)
  (assert (sgf-node-p node))
  (let ((prop-list (sgf-node-search-property node id)))
    (if prop-list
	(sgf-property-add-value (first prop-list) val)
      (sgf-node-add-property node (make-sgf-property id val)))))


(defun sgf-node-delete-property (node prop)
  (assert (sgf-node-p node))
  (cond 
   ((stringp prop)
    (setf (sgf-node-property-list node) 
	  (delete-if (lambda (p) (equal (sgf-property-id p) prop))  (sgf-node-property-list node))))
   ((sgf-property-p prop)
    (setf (sgf-node-property-list node) 
	  (delq prop (sgf-node-property-list node))))
   (t  (error "unknown type:%s" prop))))

(defun sgf-node-canonicalize (node)
  "Remove properties whose property list is nil"
  (assert (sgf-node-p node))
  (setf (sgf-node-property-list node)
	(delete-if 'sgf-property-null-value-list-p (sgf-node-property-list node))))

(defun sgf-node-delete-property-value (node id val)
  (assert (sgf-node-p node))
  (mapc (lambda (prop) (sgf-property-delete-value prop val))
	  (sgf-node-search-property node id val))
  (sgf-node-canonicalize node))

(defun sgf-node-delete-property-value-if (node id pred)
  (assert (sgf-node-p node))
  (loop for prop in (sgf-node-property-list node)
	if (equal (sgf-property-id prop) id)
	do (sgf-property-delete-value-if prop pred))
  (sgf-node-canonicalize node))

(defun sgf-node-delete-property-value-regexp (node id regexp)
  (sgf-node-delete-property-value-if node id
				     (lambda (val) (string-match regexp val))))

(defun sgf-node-value-list (node id)
  (assert (sgf-node-p node))
  (apply 'append (mapcar 'sgf-property-value-list (sgf-node-search-property node id))))



(defun sgf-node-to-string (sgf-node)
  (apply 'concat ";" (mapcar 'sgf-property-to-string (sgf-node-property-list sgf-node))))


;; methods to manipulate sgf-tree object
(defun make-sgf-tree ()
  (make-my-tree 'sgf-node-p 'sgf-node-to-string))
(defun sgf-tree-search-next-node (tree id &optional val)
  (first (member-if (lambda (node)
		      (sgf-node-search-property node id val))
		    (my-tree-child-element-list tree))))

;; Game specific operations
;; Currently Only Go (GM[1]) is supported
(defun sgf-go-num-to-alpha (num)
  (cond 
   ((and (>= num 1 ) (<= num 26))   (+ ?a  num -1) )
   ((and (>= num 27) (<= num 52))   (+ ?A  num -27) )
   (t     (error  "out of range:%s" num))))

(assert (eq (sgf-go-num-to-alpha 1)  ?a))
(assert (eq (sgf-go-num-to-alpha 26) ?z))
(assert (eq (sgf-go-num-to-alpha 27) ?A))
(assert (eq (sgf-go-num-to-alpha 52) ?Z))

(defun sgf-go-alpha-to-num (a)
  (cond 
   ((and (>= a ?a ) (<= a  ?z))     (+ 1  (- a  ?a)))
   ((and (>= a ?A)  (<= a  ?Z))     (+ 27 (- a  ?A)))
   (t     (error (format "out of range:%s" a)))))

(assert (eq (sgf-go-alpha-to-num ?a) 1))
(assert (eq (sgf-go-alpha-to-num ?z) 26))
(assert (eq (sgf-go-alpha-to-num ?A) 27))
(assert (eq (sgf-go-alpha-to-num ?Z) 52))

(defun sgf-go-coordinate (val)
  (assert (or (= (length val) 2) (and (> (length val) 2) (eq (elt val 2) ?:))))
  (list (sgf-go-alpha-to-num (elt val 0)) (sgf-go-alpha-to-num (elt val 1))))

(assert (equal (sgf-go-coordinate "ab") '(1 2)))
(assert (equal (sgf-go-coordinate "ab:label") '(1 2)))

(defun sgf-go-column (val)
  (sgf-go-alpha-to-num (elt val 0)))

(defun sgf-go-row (val)
  (sgf-go-alpha-to-num (elt val 1)))


(defun sgf-go-point (x y)
  (string (sgf-go-num-to-alpha x) (sgf-go-num-to-alpha y)))
(assert (equal (sgf-go-point 1 2) "ab"))

(defun sgf-go-node-coordinate (node)
  (let ((val-list (append (sgf-node-value-list node "B") (sgf-node-value-list node "W"))))
    (assert (or (not val-list) (eq (length val-list) 1)))
    (first val-list)))

(defun sgf-go-label (x y label)
  (format "%c%c:%s" (sgf-go-num-to-alpha x) (sgf-go-num-to-alpha y) label))


(defun sgf-go-node-place-black (node column row)
  (let ((point (sgf-go-point column row)))
    (when (sgf-node-search-property node "AE" point)
      (sgf-node-delete-property-value node "AE" point))
    (sgf-node-add-property-value node "AB" point)))

(defun sgf-go-node-place-white (node column row)
  (let ((point (sgf-go-point column row)))
    (when (sgf-node-search-property node "AE" point)
      (sgf-node-delete-property-value node "AE" point))
    (sgf-node-add-property-value node "AW" point)))

(defun sgf-go-node-place-stone (node color column row)
  (case color
    ('black (sgf-go-node-place-black node column row))
    ('white (sgf-go-node-place-white node column row))
    (t      (error (format "invalid color %s" color)))))

(defun sgf-go-node-remove-stone (node column row)
  (let ((point (sgf-go-point column row)))
    (when (and (not (sgf-node-search-property node "AB" point)) 
	       (not (sgf-node-search-property node "AW" point))
	       (not (sgf-node-search-property node "B"  point))
	       (not (sgf-node-search-property node "W"  point)))
      (sgf-node-add-property-value node "AE" point))
    (sgf-node-delete-property-value node "B" point)
    (sgf-node-delete-property-value node "W" point)
    (sgf-node-delete-property-value node "AB" point)
    (sgf-node-delete-property-value node "AW" point)))

(defun sgf-go-node-delete-markup (node column row)
  (let* ((point  (sgf-go-point column row))
	 (regexp (format "^%s" point)))
    (loop for id in sgf-markup-properties
	  do  (sgf-node-delete-property-value-regexp node id regexp))))

(defun sgf-go-node-delete-unique-markup (node column row)
  (let* ((point  (sgf-go-point column row)))
    (loop for id in sgf-unique-markup-properties
	  do  (sgf-node-delete-property-value node id point))))

(defun sgf-go-node-search-markup (node column row)
  (let* ((point (sgf-go-point column row))
	 (regexp (format "^%s" point)))
    (loop for id in sgf-markup-properties
	  if (sgf-node-search-property-value-regexp node id regexp)
	  collect id)))
(defun sgf-go-node-placed-stone-list (node  &optional color)
  "Return a list of information about the stones placed in NODE.
The information consists of a list of the form '(column row)'.
"
  (let ((coord-list
	 (cond ((eq  color 'black)
		(sgf-node-value-list node "AB"))
	       ((eq  color 'white)
		(sgf-node-value-list node "AW"))
	       (t  (append (sgf-node-value-list node "AB")
			   (sgf-node-value-list node "AW"))))))
    (mapcar  (lambda (val) (list (sgf-go-column val) (sgf-go-row val)))
	     coord-list)))
  

(defun sgf-go-node-set-triangle (node column row)
  (sgf-go-node-delete-unique-markup node column row)
  (sgf-node-add-property-value node "TR" (sgf-go-point column row)))

(defun sgf-go-node-set-circle (node column row)
  (sgf-go-node-delete-unique-markup node column row)
  (sgf-node-add-property-value node "CR" (sgf-go-point column row)))

(defun sgf-go-node-set-square (node column row)
  (sgf-go-node-delete-unique-markup node column row)
  (sgf-node-add-property-value node "SQ" (sgf-go-point column row)))

(defun sgf-go-node-set-selected (node column row)
  (sgf-go-node-delete-unique-markup node column row)
  (sgf-node-add-property-value node "SL" (sgf-go-point column row)))

(defun sgf-go-node-set-mark (node column row)
  (sgf-go-node-delete-unique-markup node column row)
  (sgf-node-add-property-value node "MA" (sgf-go-point column row)))

(defun sgf-go-node-set-label (node column row label)
  (sgf-node-delete-property-value-regexp node "LB" (format "^%s" (sgf-go-point column row)))
  (sgf-node-add-property-value node "LB" (sgf-go-label column row label)))

(defun sgf-go-node-set-comment (node comment)
  (sgf-node-delete-property node "C")
  (when (> (length comment) 0)
    (sgf-node-add-property-value node "C" comment)))

(defun sgf-go-node-get-comment (node)
  (let ((val-list (sgf-node-search-property node "C")))
    (assert (<= (length val-list) 1))
    (first val-list)))

(defun sgf-go-tree-search-next-move (tree x y)
  (let ((move-list (my-tree-search-child-node tree
					      (lambda (node) 
						(or (sgf-node-search-property node "B" (sgf-go-point x y))
						    (sgf-node-search-property node "W" (sgf-go-point x y)))))))
    (if (cdr move-list)
	(error "more than one sgf node is defined for the move at (%x,%x)" x y)
      (first move-list))))

(defun sgf-go-tree-add-move (tree color x y)
  (let ((id (case color 
	      ('black "B")
	      ('white "W")
	      (t (error "unknown color"))))
	(node (make-sgf-node)))
    (sgf-node-add-property-value node id (sgf-go-point x y))
    (my-tree-add-child tree node)))

(defun sgf-go-tree-current-player (tree)
  (let ((node   (my-tree-current-node tree))
	(parent (my-tree-parent-node  tree)))
  (cond ((sgf-node-search-property node "PL" "B") 'black) ;;PL indicates whose turn it is to play
	((sgf-node-search-property node "PL" "W") 'white)
	((sgf-node-search-property node "B")  'white)
	((sgf-node-search-property node "W")   'black)
	((and (sgf-node-p parent) (sgf-node-search-property parent "B"))  'black)
	((and (sgf-node-p parent) (sgf-node-search-property parent "W"))  'white)
	((sgf-tree-search-next-node tree "B")  'black)
	((sgf-tree-search-next-node tree "W")  'white)
	(t nil))))


(defun sgf-go-node-label-list (node)
  "Return a list of all lables at NODE. The format of each element is (column  row label)"
  (assert (sgf-node-p node))
  (mapcar  (lambda (val) (list (sgf-go-column val) (sgf-go-row val) (sgf-value-nth val 1)))
	   (sgf-node-value-list node "LB")))

(defun sgf-go-transformer (x1 x2 y1 y2)
  "affine action 
|x|    |x1 x2| |x-10|   |10|
|y| -> |y1 y2| |y-10| + |10|
"
  (lexical-let ((x1 x1) (y1 y1) (x2 x2)	(y2 y2))
    (function (lambda(x) (let ((a (first x)) (b (second x)) (rest (cddr x)))
			   (nconc (list (+ (* x1  (- a 10)) (* x2 (- b 10)) 10) (+ (* y1  (- a 10)) (* y2 (- b 10)) 10))
				  rest))))))

(defun sgf-go-tree-set-problem-how-much (tree color yose-type integer &optional fraction transform)
  "COLOR is either 'black or 'white 
YOSE-TYPE is one of 'sente 'gote 'reverse-sente
TRANSFORM is an action Z19*Z19 -> Z19*Z19 
whose  argument and  return value are  list of  2 integers.
"
  (let* ((transform   (or (and (functionp transform) transform)
			  (and (listp transform) (apply 'sgf-go-transformer transform))
			  'identity))
	 (label-list  '((1 1 "s") (1 2  "g")  (1 3 "r")
			(2 2 "10")  (2 3 "20") (2 4 "30") (2 5 "40") (2 6  "50") (2 7 "60") (2 8 "70") (2 9 "80") (2 10 "90") 
			(3 1 "0") (3 2 "1")   (3 3 "2")  (3 4 "3")  (3 5 "4")  (3 6 "5")   (3 7 "6")  (3 8 "7")  (3 9 "8")  (3 10 "9") 
			(4 1 "0") (4 2 "1/2") (4 3 "1/3") (4 4 "2/3"))))
    (assert (eq 2 (length (funcall transform '(1 2)))))
    (setq label-list (mapcar transform label-list))

    (my-tree-go-to-root tree)
    (my-tree-next-node tree)
    (my-tree-delete-all-child tree)
    
    (loop for lv in label-list
	  do (apply 'sgf-go-node-set-label (my-tree-current-node tree) lv))

    (flet ((add-move (column row) 
		     (apply 'sgf-go-tree-add-move tree color  (funcall transform (list column row)))
		     (setq color (sgf-opposite-color color))
		     (loop for lv in label-list
			   do (apply 'sgf-go-node-set-label (my-tree-current-node tree) lv))))
      (add-move 1 (case yose-type ('sente 1) ('gote 2) ('reverse-sente 3) (t (error))))
      (add-move 19 1)
      
      (when (> (/ integer 10) 0)
	(add-move 2 (+ 1 (/ integer 10)))
	(add-move 19 2))
      (add-move 3  (+ 1 (mod integer 10)))
      (add-move 19 3)

      (add-move 4  (cond ((null fraction) 1)
			 ((eq fraction 'half) 2) 
			 ((eq fraction 'one-third) 3) 
			 ((eq fraction 'two-third) 4)
			 (t (error "Unknown fraction :%s" fraction))))
      (add-move 19 4))))

    
    

  ;; end GM[1] (go)

(provide 'sgf)
