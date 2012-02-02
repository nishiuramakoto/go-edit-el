
(require 'my-tree)
(require 'cl)
(require 'sgf)

(defconst go-game-application-name-string "makogo 0.0.1")
(defvar   go-game-default-white t)

(defun go-opposite-color (color)
  (case color
    ('black 'white)
    ('white 'black)
    (t (error "unknown color"))))


(defstruct (go-stone)
  color
  property)

(defun go-stone-black-p (stone)
  (eq 'black (go-stone-color stone)))
(defun go-stone-white-p (stone)
  (eq 'white (go-stone-color stone)))
(defun go-stone-set-color (stone color)
  (setf (go-stone-color stone) color))
(defun go-stone-set-property (stone property)
  (setf (go-stone-property stone) property))



	    
(defstruct (go-game
	    (:constructor make-go-game nil)
	    (:constructor make-go-game-private )
	    )
  ;; TODO:use AVL tree instead
  
  tree
  stack
  board
  (size 19)
  (agehama-black 0)
  (agehama-white 0)
  (komi 0)
  (title nil)
  (comment nil)
  (result  nil)
  )

(defun go-game-make-default-sgf-tree ()
  (let ((new-tree (make-sgf-tree))
	(initial-node (make-sgf-node)))
    (sgf-node-add-property-value initial-node  "AP" go-game-application-name-string)
    (my-tree-add-child new-tree initial-node)
    new-tree))

(defun make-go-game (&optional sgf board-size)
  (let ((go (make-go-game-private))
	(size (or board-size 19)))
    (setf (go-game-tree go) (or sgf (go-game-make-default-sgf-tree)))
    (my-tree-go-to-root (go-game-tree go))
    (my-tree-next-node (go-game-tree go))
    (setf (go-game-stack go) (list nil))
    (setf (go-game-size  go) size)
    (setf (go-game-board go) (make-vector (* size size) nil))
    go))
  

(defun go-game-print-sgf (go)
  (concat "(" (my-tree-print-to-string (go-game-tree go)) ")"))
(defun go-game-current-node (go)
  (assert (not (my-tree-at-first-node-p (go-game-tree go))))
  (my-tree-current-node (go-game-tree go)))
(defun go-game-at-last-node (go)
  (my-tree-at-last-node-p (go-game-tree go)))
(defun go-game-next-node (go &optional node)
  (my-tree-next-node (go-game-tree go) node))
(defun go-game-prev-node (go)
  (my-tree-prev-node (go-game-tree go))
  (when (my-tree-at-first-node-p (go-game-tree go))
    (my-tree-next-node (go-game-tree go))))

(defun go-game-delete-node (go)
  (my-tree-del-node (go-game-tree go)))

(defun go-game-at-root-node (go)
  (assert (not (my-tree-at-first-node-p (go-game-tree go))))
  (not (sgf-node-p (my-tree-parent-node (go-game-tree go)))))

(defun go-game-search-child (go prop &optional val)
  (sgf-tree-search-child (go-game-tree go) prop val))
(defun go-game-search-parent-property (go prop &optional val)
  (let ((parent (my-tree-parent-node   (go-game-tree go))))
    (and parent (sgf-node-search-property parent prop val))))


(defun go-game-increment-agehama (go color)
  (case color
    ('black (incf (go-game-agehama-black go)))
    ('white (incf (go-game-agehama-white go)))
    (t (error "unknown color %s" color))))

(defun go-game-decrement-agehama (go color)
  (case color
    ('black (decf (go-game-agehama-black go)))
    ('white (decf (go-game-agehama-white go)))
    (t (error "unknown color %s" color))))


(defun go-game-sgf-search-property (go prop &optional val  val2)
  (when (and (numberp val) (numberp val2))
    (setq val (sgf-point val val2)))
  (sgf-node-search-property (go-game-current-node go) prop val))
(defun go-game-sgf-delete-property-value (go prop val &optional val2)
  (when (and (numberp val) (numberp val2))
    (setq val (sgf-point val val2)))
  (sgf-node-delete-property-value (go-game-current-node go) prop val))
(defun go-game-sgf-add-property (go prop val &optional val2)
  (when (and (numberp val) (numberp val2))
    (setq val (sgf-point val val2)))
  (sgf-node-add-property-value (go-game-current-node go) prop val))

(defun go-game-sgf-add-child (go &rest prop)
  (my-tree-add-child (go-game-tree go) (make-sgf-node prop)))


(defun go-game-board-aref (go column row)
  (decf column)
  (decf row)
  (aref (go-game-board go) (+ (* column 19) row)))

(defun go-game-board-aset (go column row x)
  (decf column)
  (decf row)
  (aset (go-game-board go) (+ (* column 19) row) x))


(defun go-game-board-ref-color (go column row)
  (let ((stone (go-game-board-aref go column row)))
    (when stone
      (go-stone-color stone))))

(defun go-game-board-set-color (go color column row)
    (when (not (go-game-board-aref go column row))
      (go-game-board-aset go column row (make-go-stone)))
    (let ((stone (go-game-board-aref go column row)))
      (go-stone-set-color stone color)))

(defun go-game-board-ref-property (go column row)
  (let ((stone (go-game-board-aref go column row)))
    (when stone
      (go-stone-property stone))))

(defun go-game-board-set-property (go prop column row)
    (when (not (go-game-board-aref go column row))
      (go-game-board-aset go column row (make-go-stone)))
    (let ((stone (go-game-board-aref go column row)))
      (go-stone-set-property stone prop)))







;; object to represent various actions during a game
;; e.g. placing a stone,capturing a stone,label a stone,etc..
;; for any action,there is an inverse action that enables undoing,
;; so that the set of strings of actions forms a free group.
(defstruct (go-game-board-action 
	    (:constructor nil)
	    (:constructor make-go-game-board-action
			  (&optional type column row pre-value post-value)))


  type 
  column
  row
  pre-value
  post-value)

(defun make-change-property-action (go prop column row)
  (let* ((pre-value (go-game-board-ref-property go column row )))
    (make-go-game-board-action 'change-property column row pre-value prop)))
(defun make-change-color-action (go color column row)
  (let* ((pre-value (go-game-board-ref-color go column row )))
    (make-go-game-board-action 'change-color column row pre-value color)))
(defun make-capture-stone-action (go column row)
  (let* ((pre-value (go-game-board-ref-color go column row )))
    (make-go-game-board-action 'capture-stone column row pre-value nil)))




(defun go-game-board-push-action-list (go)
  (push (list) (go-game-stack go))

  (let* ((parent (my-tree-parent-node (go-game-tree go)))
	 (triangle-list (if parent (sgf-node-value-list parent "TR"))))
    (loop for pt in triangle-list
	  for column = (sgf-column pt)
	  for row    = (sgf-row    pt)
	  do
	  (go-game-board-add-action go (make-change-property-action go nil column row)))))
(defun go-game-board-pop-action-list (go)
  (loop for action in (car (go-game-stack go))
	do (go-game-board-action-undo action go))
  (pop (go-game-stack go)))
  

(defun go-game-board-add-action (go action)
  (assert (consp (go-game-stack go)))
  (push action (car (go-game-stack go)))
  (go-game-board-action-execute action go))
  
(defun go-game-board-action-execute (action go)
  (let* ((column (go-game-board-action-column action))
	 (row    (go-game-board-action-row    action))
	 (action-type   (go-game-board-action-type  action))
	 (action-value  (go-game-board-action-post-value action))
	 (pre-value     (go-game-board-action-pre-value  action)))
    (case action-type
      ('change-property  (go-game-board-set-property go action-value column row ))
      ('change-color     (go-game-board-set-color    go action-value column row ))
      ('capture-stone    (go-game-board-set-color    go action-value column row )
			 (go-game-increment-agehama go (go-opposite-color pre-value)))
      (t    (error "unknown action type:%s" action-type)))))

(defun go-game-board-action-undo (action go)
  (let* ((column (go-game-board-action-column action))
	 (row    (go-game-board-action-row    action))
	 (action-type   (go-game-board-action-type  action))
	 (action-value  (go-game-board-action-pre-value action)))
    (case action-type
      ('change-property (go-game-board-set-property go action-value column row ))
      ('change-color    (go-game-board-set-color    go action-value column row ))
      ('capture-stone    (go-game-board-set-color   go action-value column row )
			 (go-game-decrement-agehama go (go-opposite-color action-value)))
      (t    (error "unknown action type:%s" action-type)))))

(defun go-game-board-execute-node (go &optional node)
  (when (not (sgf-node-p node))
    (setq node (go-game-current-node go)))
  (loop for p in (sgf-node-value-list node "TR")
	for column = (sgf-column p)
	for row    = (sgf-row p)
	do
	(go-game-board-add-action go (make-change-property-action go 'triangle column row)))

  (loop for p in (sgf-node-value-list node "AB")
	for column = (sgf-column p)
	for row    = (sgf-row p)
	do
	(go-game-board-add-action go (make-change-color-action go 'black column row)))

  (loop for p in (sgf-node-value-list node "AW")
	for column = (sgf-column p)
	for row    = (sgf-row p)
	do
	(go-game-board-add-action go (make-change-color-action go 'white column row)))
  (loop for p in (sgf-node-value-list node "B")
	for column = (sgf-column p)
	for row    = (sgf-row p)
	do
	(go-game-board-add-action go (make-change-color-action go 'black column row)))

  (loop for p in (sgf-node-value-list node "W")
	for column = (sgf-column p)
	for row    = (sgf-row p)
	do
	(go-game-board-add-action go (make-change-color-action go  'white column row)))

  )



(defun go-game-print-board (go &optional x y prop-func)
  "
Print the  goban at offset (X,Y) from the beginning of next line.
If PROP-FUNC is non-nil,it should be a function such that the form
 (funcall PROP-FUNC color column row stone-property) is valid. \n
 Its  return value is used as a list of text properties to be put at that position.
 Here , color is one of black,white or nil(no stone). \n
 stone-property is not yet defined precisely. \n
"
  (unless (numberp x) (setq x 0))
  (unless (numberp y) (setq y 0))
  (let ((board (go-game-board go))
	(ysp   (make-string y ?\n))
	(xsp   (make-string x ?  )))
    (insert ysp)
    (loop for row from 1 to 19
	  do
	  (insert "\n" xsp (sgf-num-to-alpha row))
	  (loop for col from 1 to 19
		for stone =  (go-game-board-aref go col row)
		for star  = (and (memq col '(4 10 16)) (memq row '(4 10 16)))
		for color = (and stone (go-stone-color stone))
		for property = (and stone (go-stone-property stone))
		for first-char =
		(cond
		 ((not color)              (if star ?+ ?.))
		 ((eq color 'black)        ?X)
		 ((eq color 'white)        ?O)
		 (t (error "unknow color type:%s" color)))
		for second-char =
		(cond 
		 ((not property)  ? )
		 ((eq property 'triangle) ?^)
		 ((stringp property)   (string-to-char property))
		 (t (error "unknown property type:%s" property)))
		do
		(insert  first-char second-char)
		(when (functionp prop-func)
		  (set-text-properties (- (point) 2) (point) 
				       (funcall prop-func color col row property)))))))



		     


(defun go-game-place-stone (go color column row)
  (let* ((prop   (if (eq color 'black) "AB" "AW"))
	 (action (make-change-color-action go color column row)))
    (assert (not (go-game-board-ref-color go column row)))
    (when (go-game-sgf-search-property    go "AE"  column row)
      (go-game-sgf-delete-property-value  go "AE"  column row))
    (go-game-sgf-add-property  go prop column row)
    (go-game-board-add-action go action)
    ))


(defun go-game-remove-stone (go column row)
  (let* ((color (go-game-board-ref-color go column row))
	 (prop   (if (eq color 'black) "AB" "AW"))
	 (action (make-change-color-action go nil column row)))
    (assert (eq color (go-game-board-ref-color go column row)))
    (cond ((go-game-sgf-search-property  go prop  column row)
	   (go-game-sgf-delete-property-value  go prop  column row)
	   (go-game-board-add-action go action))
	  (t
	   (go-game-sgf-add-property  go "AE"  column row)
	   (go-game-board-add-action go action)))))


(defun go-game-set-triangle (go column row)
  (assert (not (go-game-sgf-search-property go "TR"  column row)))
  (go-game-sgf-add-property go "TR"   column row)
  (go-game-board-add-action go (make-change-property-action go 'triangle column row)))


(defun go-game-remove-property (go column row)
  (go-game-sgf-delete-property-value go  "TR"  column row)
  (go-game-sgf-delete-property-value go  "CR"  column row)
  (go-game-sgf-delete-property-value go  "LB"  column row)
  (go-game-board-add-action go (make-change-property-action go nil column row)))



(defun go-game-move-stone (go color column row &optional overwrite)
  (let* ((opposite-color (go-opposite-color color))
	 (oldcolor (go-game-board-ref-color go column row))
	 (point   (sgf-point column row))
	 (prop    (make-sgf-property 
		   (if (eq color 'black) "B" "W") point)))

    (assert (sgf-property-valid-p prop))
    (case  oldcolor
      (color 
       (message  "there is already a %s stone there " color))
      (opposite-color
       (go-game-capture-stone go column row)))
    (cond  
     ((eq overwrite 'variation)  (if (go-game-check-next-move go color column row)
				     (go-game-choose-move go column row)
				   (go-game-sgf-add-child go prop)))
     ((eq overwrite nil)       (if  (go-game-check-next-move go color column row) 
				   (go-game-choose-move go column row)
				 (error "no such move:%s %d,%d" color column row)))
     ((eq overwrite t)          (go-game-sgf-add-child go prop))
     (t                 (error "Unknown value of overwrite-mode:%s" overwrite)))
    (go-game-board-push-action-list go)
    (go-game-board-execute-node go)))


(defun go-game-capture-stone (go column row)
  (go-game-board-add-action go (make-capture-stone-action go  column row)))


(defun go-game-next-move (go &optional node)
  "Step one move forward,return the moved stone"
  (when (go-game-at-last-node go)
      (error "no more moves"))
  (go-game-next-node go node)
  (go-game-board-push-action-list go)
  (go-game-board-execute-node go))


(defun go-game-choose-move (go column row)
  "Advance one move ahead that corresponds to board coordinate (COLUMN,ROW).
Return nil iff no such move is defined.
"
  (let* ((next-node (or 
		     (go-game-search-child go  "B" (sgf-point column row))
		     (go-game-search-child go  "W" (sgf-point column row)))))
    (if next-node
	(go-game-next-move go next-node)
      (error "no such move:%d,%d" column row))))
	


(defun go-game-check-next-move (go color column row)
  (let ((prop (case color
		('black "B")
		('white "W")
		(t (error "unknown color:%s" color)))))
    (go-game-search-child go prop (sgf-point column row))))

(defun go-game-last-move (go)
  (while (not (go-game-at-last-node go))
    (go-game-next-move go)))

(defun go-game-prev-move (go)
  "Roll back the game by one move,return the changing stone(captured stone or moved stone)"
  (when (go-game-at-root-node go)
      (error "Illegal op:No more previous nodes"))
  (go-game-board-pop-action-list go)
  (go-game-prev-node go))


(defun go-game-first-move (go)
  (while (not (go-game-at-root-node go))
    (go-game-prev-move go)))


(defun go-game-delete-move (go)
  (let* ((node (go-game-current-node go)))
    (go-game-prev-move go)
    (go-game-delete-node node)))


(defun go-game-current-color (go)
  (cond 
   ((or (go-game-search-child go "B") (go-game-search-parent-property go "B"))
    'white)
   ((or (go-game-search-child go "W") (go-game-search-parent-property go "W"))
    'black)
   (t
    (if go-game-default-white 'white 'black))))


(provide 'go-game)
