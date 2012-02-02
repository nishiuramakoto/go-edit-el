;; See elisp manual
;(require 'cl)
(eval-when-compile (require 'cl))

(require 'my-tree)
(require 'sgf)

;; Rule:This file should not deal with specifics of sgf format directly (value-type of a property, syntax of  property values,etc.)

(defconst go-game-application-name-string "makogo 0.0.1")
(defvar   go-game-default-white t)

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


;; TODO:use AVL tree for space-efficiency
(defstruct (go-board
	    (:constructor nil)
	    (:constructor make-go-board (&optional (size 19)
						   &aux (array (make-vector (* size size) nil)))))
  size
  array
  )
	    
(defun go-board-aref (board column row)
  (decf column)
  (decf row)
  (aref (go-board-array board) (+ row (* column (go-board-size board)))))

(defun go-board-aset (board column row x)
  (decf column)
  (decf row)
  (aset (go-board-array board) (+ row (* column (go-board-size board))) x))


(defstruct (go-game
	    (:constructor nil)
	    (:constructor make-go-game-private )
	    )
  ;; TODO:use AVL tree instead
  
  tree
  stack
  board
  (agehama-black 0)
  (agehama-white 0)
  (komi 0)
  (title nil)
  (comment nil)
  (result  nil)
  player-color
  (change-list nil)
  (info nil)
  (move-number 0)
  )

(defun go-game-make-default-sgf-tree ()
  (let ((new-tree (make-sgf-tree))
	(initial-node (make-sgf-node)))
    (my-tree-add-child new-tree initial-node)
    new-tree))

(defun make-go-game (&optional go-or-sgf )
  (let ((go (make-go-game-private))
	(sgf (cond ((go-game-p go-or-sgf) (go-game-tree go-or-sgf))
		   ((sgf-tree-p go-or-sgf) go-or-sgf)
		   ((null go-or-sgf)  (go-game-make-default-sgf-tree))
		   (t  (error "could not make go-game object")))))
    (go-game-reset-sgf go sgf)
    go))

(defun go-game-reset-sgf (go sgf)
    (setf (go-game-tree go)  sgf)
    (my-tree-go-to-root (go-game-tree go))
    (setf (go-game-stack go) (list nil))
    (setf (go-game-board go) (make-go-board 19))
    (setf (go-game-agehama-black go) 0)
    (setf (go-game-agehama-white go) 0)
    (setf (go-game-komi go) 0)
    (setf (go-game-comment go) nil)
    (setf (go-game-result go) nil)
    (setf (go-game-change-list go) nil)
    (go-game-next-move go)
    (go-game-sync-color go)
    go)
  

(defun go-game-set-problem-how-much (go color yose-type integer &optional fraction  transform) 
  "See docstring of sgf-go-tree-set-problem-how-much about TRANSFORM argument"
  (sgf-go-tree-set-problem-how-much (go-game-tree go) color  yose-type integer fraction transform)
  (go-game-reset-sgf go (go-game-tree go)))

(defun make-go-game-list-from-file (filename)
  (mapcar 'make-go-game (make-sgf-tree-list-from-file filename)))

(defun go-game-empty-p (go)
  (let* ((tree (go-game-tree go))
	 root sgf-root child-list grand-child-list)
    (setq root (my-tree-root-node tree))
    
    (setq child-list (my-tree-child-node-list tree root))
    (assert (eq 1 (length child-list)))
    (setq sgf-root (my-tree-get-element (first child-list)))
    (setq grand-child-list (my-tree-child-node-list tree (first child-list)))
    (and (not (sgf-node-property-list sgf-root))
	 (not grand-child-list))))
	 
(defun go-game-board-size (go)
  (go-board-size (go-game-board go)))


(defun go-game-clear-change-list (go)
  (setf    (go-game-change-list go) nil))

(defun go-game-tree-root-node (go)
  (let* ((tree (go-game-tree go))
	 root sgf-root child-list grand-child-list)
    (setq root (my-tree-root-node tree))
    (setq child-list (my-tree-child-node-list tree root))
    (assert (eq 1 (length child-list)))
    (setq sgf-root (my-tree-get-element (first child-list)))
    sgf-root))
  

(defun go-game-to-sgf-string (go)
  (concat "(" (my-tree-print-to-string (go-game-tree go)) "\n)\n"))
(defun go-game-tree-current-node (go)
  (assert (not (my-tree-at-first-node-p (go-game-tree go))))
  (my-tree-current-node (go-game-tree go)))
(defun go-game-tree-parent-node (go)
  (my-tree-parent-node (go-game-tree go)))

(defun go-game-tree-at-last-node (go)
  (my-tree-at-last-node-p (go-game-tree go)))

(defun go-game-tree-next-node (go &optional node)
  (my-tree-next-node (go-game-tree go) node))

(defun go-game-tree-prev-node (go)
  (my-tree-prev-node (go-game-tree go))
  (when (my-tree-at-first-node-p (go-game-tree go))
    (my-tree-next-node (go-game-tree go))))

(defun go-game-tree-delete-node (go)
  (my-tree-del-node (go-game-tree go)))

(defun go-game-tree-at-root-node (go)
  (assert (not (my-tree-at-first-node-p (go-game-tree go))))
  (not (sgf-node-p (my-tree-parent-node (go-game-tree go)))))


(defun go-game-increment-move-number (go)
  (incf (go-game-move-number go)))

(defun go-game-decrement-move-number (go)
  (decf (go-game-move-number go)))

(defun go-game-increment-agehama (go color)
  (case color
    ('black (incf (go-game-agehama-black go)))
    ('white (incf (go-game-agehama-white go)))
    (t (error "go-game-increment-agehama:unknown color %s" color))))

(defun go-game-decrement-agehama (go color)
  (case color
    ('black (decf (go-game-agehama-black go)))
    ('white (decf (go-game-agehama-white go)))
    (t (error "go-game-decrement-agehama:unknown color %s" color))))


(defun go-game-set-game-info (go info-type info)
  (push (cons info-type info) (go-game-info go)))

(defun go-game-tree-add-child (go &rest prop)
  (my-tree-add-child (go-game-tree go) (make-sgf-node prop)))



(defun go-game-node-coordinate (node)
  (assert (sgf-node-p node))
  (sgf-go-node-coordinate node))

(defun go-game-board-aref (go column row)
  (go-board-aref (go-game-board go) column row))

(defun go-game-board-aset (go column row x)
  (go-board-aset (go-game-board go) column row x))


(defun go-game-board-ref-color (go column row)
  (let ((stone (go-game-board-aref go column row)))
    (when stone
      (go-stone-color stone))))

(defun go-game-board-set-color (go color column row)
  (push (list column row) (go-game-change-list  go))
  (when (not (go-game-board-aref go column row))
    (go-game-board-aset go column row (make-go-stone)))
  (let ((stone (go-game-board-aref go column row)))
    (go-stone-set-color stone color)))

(defun go-game-board-ref-property (go column row)
  (let ((stone (go-game-board-aref go column row)))
    (when stone
      (go-stone-property stone))))

(defun go-game-board-set-property (go prop column row)
  (push (list column row) (go-game-change-list  go))
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

(defun go-game-valid-color-p (color)
  (memq color '(black white)))

(defun make-change-property-action (go prop column row)
  (let* ((pre-value (go-game-board-ref-property go column row )))
    (make-go-game-board-action 'change-property column row pre-value prop)))
(defun make-change-color-action (go color column row)
  (let* ((pre-value (go-game-board-ref-color go column row )))
    (make-go-game-board-action 'change-color column row pre-value color)))
(defun make-capture-stone-action (go column row)
  (let* ((pre-value (go-game-board-ref-color go column row )))
    (assert (go-game-valid-color-p pre-value))
    (make-go-game-board-action 'capture-stone column row pre-value nil)))
(defun make-set-comment-action (go &optional text)
  (let* ((pre-value (go-game-comment go )))
    (make-go-game-board-action   'set-comment  nil nil pre-value text)))

(defun go-game-board-push-action-list (go)
  (push (list) (go-game-stack go))
  (let ((parent (my-tree-parent-node (go-game-tree go))))
    (when (sgf-node-p parent)
      (loop for prop in (sgf-node-property-list parent)
	    do (loop for val in (sgf-property-value-list prop)
		     do (go-game-board-add-action go  (go-game-board-make-undo-action-from-sgf-property go  (sgf-property-id prop) val)))))))


(defun go-game-board-pop-action-list (go)
  (loop for action in (car (go-game-stack go))
	do (go-game-board-action-execute-undo action go))
  (pop (go-game-stack go)))
  

(defun go-game-board-add-action (go action)
  (assert (consp (go-game-stack go)))
  (when action
    (push action (car (go-game-stack go)))
    (go-game-board-action-execute action go)))
  
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
			 (assert (memq pre-value '(black white)))
			 (go-game-increment-agehama go (sgf-opposite-color pre-value)))
      ('set-comment      (setf (go-game-comment go) action-value))
      (t    (error "unknown action type:%s" action-type)))))


(defun go-game-board-action-execute-undo (action go)
  (let* ((column (go-game-board-action-column action))
 	 (row    (go-game-board-action-row    action))
 	 (action-type   (go-game-board-action-type  action))
 	 (action-value  (go-game-board-action-pre-value action)))
    (case action-type
      ('change-property (go-game-board-set-property go action-value column row ))
      ('change-color    (go-game-board-set-color    go action-value column row ))
      ('capture-stone    (go-game-board-set-color   go action-value column row )
			 (assert (memq action-value '(black white)))
 			 (go-game-decrement-agehama go (sgf-opposite-color action-value)))
      ('set-comment      (setf (go-game-comment go) action-value))
      (t    (error "unknown action type:%s" action-type)))))
 


;; The following two functions are converters from sgf-property to go-board-action,
;; so it  must know the value type of each sgf properties.
(defun go-game-board-make-action-from-sgf-property (go id val)
  (cond
   ((string= id "AB")   (make-change-color-action    go 'black    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "B")    (make-change-color-action    go 'black    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "AW")   (make-change-color-action    go 'white    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "W")    (make-change-color-action    go 'white    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "C")    (make-set-comment-action     go val))
   ((string= id "CR")   (make-change-property-action go 'circle    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "DD")   (make-change-property-action go 'dim       		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "LB")   (make-change-property-action go (sgf-value-nth val 1) 	(sgf-go-column val) (sgf-go-row val)))
   ((string= id "MA")   (make-change-property-action go 'mark         		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "SL")   (make-change-property-action go 'select    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "SQ")   (make-change-property-action go 'square    		(sgf-go-column val) (sgf-go-row val)))
   ((string= id "TR")   (make-change-property-action go 'triangle  		(sgf-go-column val) (sgf-go-row val)))
   ;; this is already handled correctly 
   ((string= id "PL")   nil)
   ;; root properties should be dealt with at object creation time
   ((string= id "AP")   nil)
   ((string= id "CA")   nil)
   ((string= id "FF")   nil)
   ((string= id "GM")   nil)
   ((string= id "ST")   nil)
   ((string= id "SZ")   nil)

   ;; game info properties
   ((string= id "AN")   (go-game-set-game-info go "annotator" val) nil)
   ((string= id "BR")   (go-game-set-game-info go "black rank" val) nil)
   ((string= id "BT")   (go-game-set-game-info go "black team" val) nil)
   ((string= id "CP")   (go-game-set-game-info go "copyright" val) nil)
   ((string= id "DT")   (go-game-set-game-info go "date" val) nil)
   ((string= id "EV")   (go-game-set-game-info go "event" val) nil)
   ((string= id "GN")   (go-game-set-game-info go "game name" val) nil)
   ((string= id "GC")   (go-game-set-game-info go "game comment" val) nil)
   ((string= id "ON")   (go-game-set-game-info go "opening" val) nil)
   ((string= id "OT")   (go-game-set-game-info go "byo-yomi" val) nil)
   ((string= id "PB")   (go-game-set-game-info go "black player" val) nil)
   ((string= id "PC")   (go-game-set-game-info go "place" val) nil)
   ((string= id "PW")   (go-game-set-game-info go "white player" val) nil)
   ((string= id "RE")   (go-game-set-game-info go "result" val) nil)
   ((string= id "RO")   (go-game-set-game-info go "round" val) nil)
   ((string= id "RU")   (go-game-set-game-info go "rule" val) nil)
   ((string= id "SO")   (go-game-set-game-info go "source" val) nil)
   ((string= id "TM")   (go-game-set-game-info go "time limit" val) nil)
   ((string= id "US")   (go-game-set-game-info go "user" val) nil)
   ((string= id "WR")   (go-game-set-game-info go "white rank" val) nil)
   ((string= id "WT")   (go-game-set-game-info go "white team" val) nil)

   ((string= id "HA")   (go-game-set-game-info go "handicap" val) nil)
   ((string= id "KM")   (go-game-set-game-info go "komi" val) nil)
   (t  (message "unknown property:%s[%s]" id val)
       nil)))
  
  
(defun go-game-board-make-undo-action-from-sgf-property (go id val)
  (cond 
   ((member id  sgf-markup-properties)   (make-change-property-action go nil    (sgf-go-column val) (sgf-go-row val)))
   ((string= id "C")    (make-set-comment-action     go nil))
   (t  nil)))

(defun go-game-board-execute-node (go &optional node)
  (when (not (sgf-node-p node))
    (setq node (go-game-tree-current-node go)))
  (loop for prop in (sgf-node-property-list node)
	do (loop for val in (sgf-property-value-list prop)
		 do (go-game-board-add-action go (go-game-board-make-action-from-sgf-property go  (sgf-property-id prop) val)))))



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
	  (insert "\n" xsp (sgf-go-num-to-alpha row))
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
  (let* ((action (make-change-color-action go color column row))
	 (node   (go-game-tree-current-node go)))
    (assert (not (go-game-board-ref-color go column row)))
    (sgf-go-node-place-stone node color column row)
    (go-game-board-add-action go action)
    ))


(defun go-game-remove-stone (go column row)
  (let* ((node   (go-game-tree-current-node go))
	 (action (make-change-color-action go nil column row))
	 (color (go-game-board-ref-color go column row)))
    (assert color)
    (sgf-go-node-remove-stone node column row)
    (go-game-board-add-action go action)))

(defun go-game-remove-stone-all (go &optional color)
  (let* ((node (go-game-tree-current-node go))
	 (stone-list (sgf-go-node-placed-stone-list node color)))
    (loop for stone in stone-list
	  do 
	  (assert (and (numberp (first stone)) (numberp (second stone))))
	  (go-game-remove-stone go (first stone) (second stone)))))

(defun go-game-delete-markup (go column row)
  (sgf-go-node-delete-markup (go-game-tree-current-node go) column row)
  (go-game-board-add-action go (make-change-property-action go nil column row)))

(defun go-game-search-markup (go column row)
  (sgf-go-node-search-markup (go-game-tree-current-node go) column row))

(defun go-game-set-triangle (go column row)
  (sgf-go-node-set-triangle  (go-game-tree-current-node go) column row)
  (go-game-board-add-action go (make-change-property-action go 'triangle column row)))

(defun go-game-toggle-triangle (go column row)
  (if (go-game-search-markup go column row)
      (go-game-delete-markup go column row)
    (go-game-set-triangle go column row)))

(defun go-game-set-circle (go column row)
  (sgf-go-node-set-circle  (go-game-tree-current-node go) column row)
  (go-game-board-add-action go (make-change-property-action go 'circle column row)))

(defun go-game-toggle-circle (go column row)
  (if (go-game-search-markup go column row)
      (go-game-delete-markup go column row)
    (go-game-set-circle go column row)))

(defun go-game-set-square (go column row)
  (sgf-go-node-set-square  (go-game-tree-current-node go) column row)
  (go-game-board-add-action go (make-change-property-action go 'square column row)))

(defun go-game-toggle-square (go column row)
  (if (go-game-search-markup go column row)
      (go-game-delete-markup go column row)
    (go-game-set-square go column row)))

(defun go-game-set-mark (go column row)
  (sgf-go-node-set-mark  (go-game-tree-current-node go) column row)
  (go-game-board-add-action go (make-change-property-action go 'mark column row)))

(defun go-game-toggle-mark (go column row)
  (if (go-game-search-markup go column row)
      (go-game-delete-markup go column row)
    (go-game-set-mark go column row)))

(defun go-game-set-label (go column row &optional label)
  (go-game-delete-markup go column row)
  (when (> (length label) 0)
    (sgf-go-node-set-label  (go-game-tree-current-node go) column row label)
    (go-game-board-add-action go (make-change-property-action go label column row))))

(defun go-game-clear-all-labels (go)
  (loop for (x y label) in (sgf-go-node-label-list (go-game-tree-current-node go))
	do (go-game-set-label go x y)))

(defun go-game-set-comment (go text)
  (sgf-go-node-set-comment (go-game-tree-current-node go) text)
  (go-game-board-add-action go  (make-set-comment-action go text)))


(defun go-game-next-move (go &optional node)
  "Step one move forward,return the moved stone"
  (when (go-game-tree-at-last-node go)
      (error "no more moves"))
  (go-game-tree-next-node go node)
  (go-game-board-push-action-list go)
  (go-game-board-execute-node go)
  (go-game-increment-move-number go)
  (go-game-sync-color go))

(defun go-game-prev-move (go)
  "Roll back the game by one move,return the changing stone(captured stone or moved stone)"
  (when (go-game-tree-at-root-node go)
      (error "Illegal op:No more previous nodes"))

  (go-game-board-pop-action-list go)
  (go-game-tree-prev-node go)
  (go-game-decrement-move-number go)
  (go-game-sync-color go))

(defun go-game-check-next-move (go column row)
  (sgf-go-tree-search-next-move (go-game-tree go)  column row))

(defun go-game-choose-move (go column row)
  "Advance one move ahead that corresponds to board coordinate (COLUMN,ROW).
Return nil iff no such move is defined.
"
  (let* ((next-node (sgf-go-tree-search-next-move (go-game-tree go) column row)))
    (if next-node
	(go-game-next-move go next-node)
      (error "go-game-choose-move:no such move:%d,%d" column row))))

(defun go-game-add-move (go color column row )
  (assert (memq color '(black white)))
  (assert (not (go-game-check-next-move go column row)) nil (format "go-game-add-move:move (%s,%s,%s) exists" color column row))
  (sgf-go-tree-add-move (go-game-tree go) color column row)
  (go-game-board-push-action-list go)
  (go-game-board-execute-node go)
  (go-game-increment-move-number go)
  (go-game-sync-color go))

(defun go-game-move-stone (go column row &optional overwrite)
  (assert (memq (go-game-player-color go) '(black white)))

  (let* ((color          (go-game-player-color go))
	 (opposite-color (sgf-opposite-color color))
	 (oldcolor (go-game-board-ref-color go column row)))
    (case  oldcolor
      (color 
       (message  "there is already a %s stone there " color))
      (opposite-color
       (go-game-capture-stone go column row)))
    (cond  
     ((eq overwrite 'variation)          
      (if (go-game-check-next-move go column row)
	  (go-game-choose-move go column row)
	(go-game-add-move go color column row)))
     ((eq overwrite nil)
      (if  (go-game-check-next-move go  column row) 
	  (go-game-choose-move go column row)
	(error "go-game-move-stone:no such move:%s %d,%d" color column row)))
     ((eq overwrite t)   
      (if (go-game-check-next-move go column row)
	  (go-game-delete-next-move go column row))
      (go-game-add-move go color column row))
     (t                 (error "Unknown value of overwrite-mode:%s" overwrite)))
    ))


(defun go-game-capture-stone (go column row)
  (go-game-board-add-action go (make-capture-stone-action go  column row)))

(defun go-game-next-move-list (go)
  (let* ((tree (go-game-tree go))
 	 (children (my-tree-child-element-list tree)))
    children))

(defun go-game-get-next-variation (go)
  (let* ((tree (go-game-tree go)))
    (my-tree-get-next-sibling-node tree)))
(defun go-game-get-prev-variation (go)
  (let* ((tree (go-game-tree go)))
    (my-tree-get-prev-sibling-node tree)))

(defun go-game-next-variation (go)
  (let* ((node (go-game-get-next-variation go)))
    (go-game-prev-move go)
    (go-game-next-move go node)))

(defun go-game-prev-variation (go)
  (let* ((node (go-game-get-prev-variation go)))
    (go-game-prev-move go)
    (go-game-next-move go node)))


(defun go-game-alternate-move-list (go)
  (let* ((tree (go-game-tree go))
 	 (sibling (my-tree-sibling-element-list tree)))
    sibling))


(defun go-game-last-move (go)
  (while (not (go-game-tree-at-last-node go))
    (go-game-next-move go)))


(defun go-game-first-move (go)
  (while (not (go-game-tree-at-root-node go))
    (go-game-prev-move go)))


(defun go-game-delete-move (go)
  (let* ((tree (go-game-tree go))
	 (node (my-tree-top tree)))
    (go-game-prev-move go)
    (my-tree-del-child tree node)))

(defun go-game-delete-next-move (go column row)
  (go-game-choose-move go column row)
  (go-game-delete-move go))

(defun go-game-current-player (go)
  (or (sgf-go-tree-current-player (go-game-tree go))
      (and go-game-default-white 'white)  'black))

(defun go-game-change-color (go &optional color)
  (assert (memq color (list 'black 'white nil)))
  (if (memq color '(black white))
      (setf (go-game-player-color go) color)
    (setf (go-game-player-color go) (sgf-opposite-color (go-game-player-color go)))))

(defun go-game-sync-color (go)
  (setf (go-game-player-color go) (go-game-current-player go)))


(defun go-game-inherit-label (go)
  (when (not (go-game-tree-at-root-node go))
    (loop for (x y label) in (sgf-go-node-label-list (go-game-tree-parent-node go))
	  do (go-game-set-label go x y label))))


(provide 'go-game)
