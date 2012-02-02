
(require 'cl)
(require 'my-matrix)
(require 'gnugo-xpms)
(require 'go-game)

;; TODO: To make this package more general so that other board game programs get benefit from this
(defvar game-board-xpms gnugo-xpms)
(defvar game-board-force-redraw nil)

(defstruct (game-board
	    (:constructor nil)
	    (:constructor make-go-board ( &key (size 19)  &aux (column size) &aux (row size) 
					       (position-matrix  (make-my-matrix column row nil))
					       (overlay-matrix   (make-my-matrix column row nil)))))
  column
  row
  position-matrix
  overlay-matrix)

(defun game-board-draw (board go)
  "public method to draw a game"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (loop for j from 1 to (game-board-column board)
	  do
	  (loop for i from 1 to (game-board-row board)
		for piece =  (go-game-board-aref go i j)
		do  
		(insert ".")
		(mset (game-board-position-matrix board) i j (list (1- (point)) (point)))
		(game-board-set-overlay board piece i j (1- (point)) (point))
		)
	  (insert "\n"))))

(defun game-board-update-at (board piece i j)
  "method to update a graphic at (i,j)"
  (let* ((pos (mref (game-board-position-matrix board) i j))
	 (beg (first pos))
	 (end (second pos))
	 (inhibit-read-only t))
    (game-board-set-overlay board piece i j beg end)))

(defun game-board-update (board go)
  "method to update a graphic at changed points"
  (loop for (x y) in (go-game-change-list go)
	do  (game-board-update-at board (go-game-board-aref go x y) x y))
  (go-game-clear-change-list go))
    

(defun game-board-position (board column row)
  (overlay-start (mref (game-board-overlay-matrix board) column row)))

(defun game-board-coord-at (board &optional pos)
  (setq pos (or pos (point)))
  (list (get-char-property pos 'board-column) 
	(get-char-property pos 'board-row)))

(defun game-board-column-at (board &optional pos)
  (setq pos (or pos (point)))
  (get-char-property pos 'board-column))

(defun game-board-row-at (board &optional pos)
  (setq pos (or pos (point)))
  (get-char-property pos 'board-row))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private method section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-create-image (type place)
  (let* ((xpm-data (if (listp game-board-xpms)
		       game-board-xpms
		     (eval game-board-xpms)))
	 (xpm   (cdr (assoc (cons type place) xpm-data))))
;    (message "go-create-image %s %s" type place)
    (assert xpm)
    (create-image (plist-get (cdr xpm) :data) 'xpm t :ascent 'center)))


;; The image cache is global
(defvar go-stone-image-cache nil)
(defun  go-board-get-image (image-type)
  (if (not (hash-table-p go-stone-image-cache))
      (setq  go-stone-image-cache (make-hash-table :test 'equal)))
  (if (not (gethash image-type go-stone-image-cache))
      (let ((newimage  (go-create-image (second image-type) (first image-type))))
	(puthash image-type newimage go-stone-image-cache)
	newimage)
    (gethash image-type go-stone-image-cache)))
      
(defun  go-board-get-image-type (piece column row)
  (let* ((place (case row
		  (1  (case column (1 1) (19 3) (t 2)))
		  (19 (case column (1 7) (19 9) (t 8)))
		  (t  (case column (1 4) (19 6) (t 5)))))
	 (hoshi-p (and (memq column '(4 10 16)) (memq row '(4 10 16))))
	 (point-p (and piece (eq (go-stone-property piece) 'triangle)))
	 (type  (case (and (go-stone-p piece) (go-stone-color piece))
		  ('black (if point-p 'bpmoku 'bmoku))
		  ('white (if point-p 'wpmoku 'wmoku))
		  (t    (if hoshi-p 'hoshi   'empty)))))
    (list place type (% column 2))))


(defun game-board-set-overlay (board piece i j beg end)
  (let* ((overlay  (mref (game-board-overlay-matrix board) i j))
	 (old-image-type  (if (overlayp overlay) (overlay-get overlay 'board-image) nil))
	 (image-type      (go-board-get-image-type piece i j)))
    (if (not (overlayp overlay))
	(let ((image  (go-board-get-image image-type)))
	  (assert image)
	  (setq overlay (make-overlay beg end))
	  (mset (game-board-overlay-matrix board) i j overlay)
	  (overlay-put overlay 'board-column i)
	  (overlay-put overlay 'board-row    j)
	  (overlay-put overlay 'board-image image-type)
	  (overlay-put overlay 'display image))
      (move-overlay overlay beg end)
      (if  (or (not (equal old-image-type  image-type)) game-board-force-redraw)
	  (progn
	    (overlay-put overlay 'board-image  image-type)
	    (overlay-put overlay 'display (go-board-get-image image-type)))
	))))



(provide 'game-board)
