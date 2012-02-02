
;; See elisp manual
;(require 'cl)
(eval-when-compile (require 'cl))

(require 'my-matrix)
(require 'gnugo-xpms)
(require 'go-game)

;; TODO: To make this package more general so that other board game programs get benefit from this
(defvar goban-xpms gnugo-xpms)
(defvar goban-force-redraw nil)
(defvar goban-image-data-path "~/src/myelisp/images")

(defstruct (goban
	    (:constructor nil)
	    (:constructor make-goban ( &key (size 19)  &aux (column size) &aux (row size) 
					       (position-matrix  (make-my-matrix column row nil))
					       (overlay-matrix   (make-my-matrix column row nil)))))
  column
  row
  position-matrix
  overlay-matrix
  )

(defun goban-draw (goban go)
  "public method to draw a game"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (loop for j from 1 to (goban-column goban)
	  do
	  (loop for i from 1 to (goban-row goban)
		for stone =  (go-game-board-aref go i j)
		do  
		(insert ".")
		(mset (goban-position-matrix goban) i j (list (1- (point)) (point)))
		(goban-set-overlay goban stone i j (1- (point)) (point))
		)
	  (insert "\n"))))

(defun goban-update-at (goban stone i j)
  "method to update a graphic at (i,j)"
  (let* ((pos (mref (goban-position-matrix goban) i j))
	 (beg (first pos))
	 (end (second pos))
	 (inhibit-read-only t))
    (goban-set-overlay goban stone i j beg end)))

(defun goban-update (goban go)
  "method to update a graphic at changed points"
  (loop for (x y) in (go-game-change-list go)
	do  (goban-update-at goban (go-game-board-aref go x y) x y))
  (go-game-clear-change-list go))
    

(defun goban-position (goban column row)
  (overlay-start (mref (goban-overlay-matrix goban) column row)))

(defun goban-coord-at (goban &optional pos)
  (setq pos (or pos (point)))
  (list (get-char-property pos 'goban-column) 
	(get-char-property pos 'goban-row)))

(defun goban-column-at (goban &optional pos)
  (setq pos (or pos (point)))
  (get-char-property pos 'goban-column))

(defun goban-row-at (goban &optional pos)
  (setq pos (or pos (point)))
  (get-char-property pos 'goban-row))

(defun goban-color-at (goban &optional pos)
  (setq pos (or pos (point)))
  (go-stone-color (get-char-property pos 'goban-stone)))

(defun goban-property-at (goban &optional pos)
  (setq pos (or pos (point)))
  (go-stone-property (get-char-property pos 'goban-stone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private method section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun  goban-get-image-type (stone column row)
  (let* ((place (case row
		  (1  (case column (1 1) (19 3) (t 2)))
		  (19 (case column (1 7) (19 9) (t 8)))
		  (t  (case column (1 4) (19 6) (t 5)))))
	 (hoshi-p (and (memq column '(4 10 16)) (memq row '(4 10 16))))
;	 (point-p (and stone (eq (go-stone-property stone) 'triangle)))
	 (point-p (and stone (go-stone-property stone)))
	 (type  (case (and (go-stone-p stone) (go-stone-color stone))
		  ('black (if point-p 'bpmoku 'bmoku))
		  ('white (if point-p 'wpmoku 'wmoku))
		  (t    (if hoshi-p 'hoshi   'empty)))))
    (list place type (% column 2))))


(defun goban-get-image-filename (color prop column row)
  (let* ((property-type (cond
			 ((eq prop nil) nil)
			 ((eq prop 'triangle) "TR")
			 ((eq prop 'circle)   "CR")
			 ((eq prop 'square)   "SQ")
			 ((eq prop 'mark)     "LB-X")
			 ((stringp prop)      (format "LB-%s" prop))
			 (t  (error "unknown property type:%s" prop))))
	 (place    (case row
		     (1  (case column (1 1) (19 3) (t 2)))
		     (19 (case column (1 7) (19 9) (t 8)))
		     (t  (case column (1 4) (19 6) (t 5)))))
	 (hoshi-p (and (memq column '(4 10 16)) (memq row '(4 10 16))))

	 (base-type (case color
		      ('black "bmoku")
		      ('white "wmoku")
		      (t      (if (and hoshi-p (not property-type))  "hoshi" "empty")))))
    (if property-type
	(format "%s-0-%s.xpm" base-type  property-type)
      (format "%s-%s.xpm" base-type place))))
  

(defun goban-create-image (type place)
  (let* ((xpm-data (if (listp goban-xpms)
		       goban-xpms
		     (eval goban-xpms)))
	 (xpm   (cdr (assoc (cons type place) xpm-data))))
    (create-image (plist-get (cdr xpm) :data) 'xpm t :ascent 'center)))


;; The image cache is global
(defvar goban-image-cache nil)
(defun goban-init-image-cache (&optional force-reset)
  (when (not (hash-table-p goban-image-cache))
    (setq  goban-image-cache (make-hash-table :test 'equal :size 300)))
  (when force-reset
    (clrhash goban-image-cache)))

(defun  goban-get-image2 (stone column row)
  (goban-init-image-cache)
  (let* ((image-type (goban-get-image-type stone column row))
	 (image (gethash image-type goban-image-cache)))
    (if (not image)
	(let ((newimage  (goban-create-image (second image-type) (first image-type))))
	  (puthash image-type newimage goban-image-cache)
	  newimage)
      image)))

(defun  goban-get-image (stone column row)
  (goban-init-image-cache)
  (let* ((file   (goban-get-image-filename (and stone (go-stone-color stone)) (and stone (go-stone-property stone)) column row))
	 (key    (list file (% column 2)))
	 (image (gethash key goban-image-cache)))
    (if (not image)
	(let ((newimage  (create-image (concat goban-image-data-path "/" file))))
	  (assert newimage)
	  (puthash key newimage goban-image-cache)
	  newimage)
	image)))




(defun goban-set-overlay (goban stone i j beg end)
  (let* ((overlay  (mref (goban-overlay-matrix goban) i j))
	 (old-stone  (if (overlayp overlay) (overlay-get overlay 'goban-stone) nil)))
    (if (not (overlayp overlay))
	(progn
	  (setq overlay (make-overlay beg end))
	  (mset (goban-overlay-matrix goban) i j overlay)
	  (overlay-put overlay 'goban-column i)
	  (overlay-put overlay 'goban-row    j)
	  (overlay-put overlay 'goban-stone (copy-go-stone stone))
	  (overlay-put overlay 'display (goban-get-image stone i j)))
      (move-overlay overlay beg end)
      (when  (or (not (equal old-stone  stone)) goban-force-redraw)
	(overlay-put overlay 'goban-stone (copy-go-stone stone))
	(overlay-put overlay 'display   (goban-get-image stone i j)))
      )))


(defun goban-refresh (goban go)
  (let ((p (point))
	(goban-force-redraw t))
    (goban-draw goban go)
    (goto-char p)))


(provide 'goban)
