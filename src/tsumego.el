
;; code for testing

(require 'cl)
(require 'go-game)
(require 'gnugo-xpms)
(require 'dll) ;; for doubly linked list
(require 'my-matrix)
(require 'mylib)

(defvar tsumego-display-image-flag nil)
(defvar tsumego-overwrite-move nil)
(defvar tsumego-mode-hook nil)
(defvar tsumego-exit-hook nil)
(defvar tsumego-board-coord '(0  0))
(defvar tsumego-initial-position '(4 4))
(defvar tsumego-auto-save-idle-time nil
  "An interger or floating point value causes auto saving in that interval")
(defvar tsumego-verbose-message nil)
(defvar tsumego-title-prefix "")
(defvar tsumego-error-threshold 5)
(defvar tsumego-cursor-type 'box)

;; The image cache is global
(defvar tsumego-image-cache (make-hash-table :test 'equal))
;; interval buffer local variables


(defvar-local tsumego-board-list )
(defvar-local tsumego-board-list-iterator)
(defvar-local tsumego-board-number 0)
(defvar-local tsumego-board-number-string)

(defvar-local tsumego-state)
(defvar-local tsumego-sgf-default-filename)
(defvar-local tsumego-sgf-auto-save-filename)
(defvar-local tsumego-current-color 'black)
(defvar-local tsumego-current-color-string)
(defvar-local tsumego-overwrite-mode-string)
(defvar-local tsumego-mouse-mode-string)
(defvar-local tsumego-idle-timer)
(defvar-local tsumego-mouse-prev-error-count 0)
(defvar-local tsumego-mouse-next-error-count 0)
(defvar-local tsumego-need-auto-save t)

(defvar tsumego-mode-keyboard-map
  (let ((map  (make-sparse-keymap)))
    (define-key map "h"        	'backward-char)
    (define-key map "j"    	'next-line)
    (define-key map "k"    	'previous-line)
    (define-key map "l"    	'forward-char)
    (define-key map "i"      	'tsumego-toggle-display-image)
    (define-key map " "      	'tsumego-place-current-color)
    (define-key map "\C-d"   	'tsumego-remove-stone)
    (define-key map "c"      	'tsumego-change-color)
    (define-key map "="      	'tsumego-view-position)
    (define-key map "\C-l"   	'tsumego-refresh)
    (define-key map "s"      	'tsumego-write-sgf-file)
    (define-key map "S"      	'tsumego-write-to-default-sgf-file)
    (define-key map "q"      	'tsumego-quiz-test)
    (define-key map "g"      	'tsumego-goto-board)
    (define-key map [insert] 	'tsumego-toggle-overwrite)
    (define-key map "\C-m"   	'tsumego-new-board)
    (define-key map [prior]  	'tsumego-prev-board)
    (define-key map [next]  	'tsumego-next-board)
    (define-key map [home]  	'tsumego-first-move)
    (define-key map [end]  	'tsumego-last-move)
    map)
  "keymap to edit tsumego.")

(defun tsumego-quiz-test ()
  (interactive)
  (save-excursion 
    (let* ((buffer (current-buffer))
	   (result (tsumego-quiz (tsumego-current-board))))
      (switch-to-buffer buffer)
      (message "quiz result=%s" result))))
  
;; Mouse commands
(defmacro tsumego-mouse-generic-command (command)
  "Return a function that sets point where a mouse event has happened,and do COMMAND. "
  `(lambda (e)
     (interactive "@e")
     (tsumego-message "mouse event:%s" e)
     (mouse-set-point e)
     (call-interactively ,command) ))


(defvar tsumego-mode-mouse-map-list
  (loop for map in
	(list 
	 (let ((map  (make-sparse-keymap)))
	   (define-key map [down-mouse-1] (tsumego-mouse-generic-command 'tsumego-place-black))
	   (define-key map [down-mouse-3] (tsumego-mouse-generic-command 'tsumego-place-white))
	   (define-key map [mouse-3] 'undefined)
	   (define-key map [down-mouse-4] 'tsumego-mouse-prev)
	   (define-key map [down-mouse-5] 'tsumego-mouse-next)
	   (define-key map [mouse-4] 'undefined)
	   (define-key map [mouse-5] 'undefined)
	   map)
	 (let ((map  (make-sparse-keymap)))
	   (define-key map [down-mouse-1]         (tsumego-mouse-generic-command 'tsumego-move))
	   (define-key map [down-mouse-3]         (tsumego-mouse-generic-command 'tsumego-capture))
	   (define-key map [double-down-mouse-3]  'tsumego-change-color)
	   (define-key map [down-mouse-4] 'tsumego-mouse-prev)
	   (define-key map [down-mouse-5] 'tsumego-mouse-next)
	   (define-key map [mouse-4] 'undefined)
	   (define-key map [mouse-5] 'undefined)
	   map)
	 )
	do
	(set-keymap-parent map tsumego-mode-keyboard-map)
	(define-key map [(mouse-2)] 'tsumego-change-mouse-mode)
	collect map))

  
(assert (not (memq nil (mapcar (lambda (map) (eq (keymap-parent map) tsumego-mode-keyboard-map))
			       tsumego-mode-mouse-map-list))))
	       
(defvar tsumego-mouse-mode 0)
(make-variable-buffer-local 'tsumego-mouse-mode)

(defun tsumego-change-mouse-mode (&optional mode)
  (interactive)
  (let ((inhibit-read-only t))
    (if mode
	(setq tsumego-mouse-mode (% mode (length tsumego-mode-mouse-map-list)))
      (setq tsumego-mouse-mode (% (+ tsumego-mouse-mode 1) (length tsumego-mode-mouse-map-list))))
    (use-local-map (nth tsumego-mouse-mode tsumego-mode-mouse-map-list))
    (tsumego-show-status)))


(defun tsumego-message (&rest args)
  (when tsumego-verbose-message
    (apply 'message args)))

;; Command definitions

(defmacro defun-tsumego-threshold-func (func-name counter-name usual-func emergency-func)
  `(defun ,func-name ()
     ,(format "Usually just execute %s,but too many successive errors cause to call %s.
Note that this function is automatically generated by the mcaro defun-tsumego-threshold-func." usual-func  emergency-func)
     (interactive)
     (condition-case sig
	 (progn
	   (,usual-func)
	   (setf ,counter-name 0))
       (error 
	(message "signal:%s" sig)
	(incf ,counter-name)
	(when (>  ,counter-name  tsumego-error-threshold)
	  (,emergency-func)
	  (setf ,counter-name 0))))))


(defun-tsumego-threshold-func tsumego-mouse-prev tsumego-mouse-prev-error-count tsumego-prev-move tsumego-prev-board)
(defun-tsumego-threshold-func tsumego-mouse-next tsumego-mouse-next-error-count tsumego-next-move tsumego-next-board)
	   


(defun tsumego-current-line ()
  (count-lines (point-min) (point)))
(defun tsumego-current-column ()
  (- (point)  (line-beginning-position)))


(defun tsumego-previous-row (&optional n)
  (interactive)
  (let ((col (tsumego-current-column)))
    (if n (forward-line (- n))
      (forward-line -1))
    (beginning-of-line)
    (forward-char col)))

(defun tsumego-next-row (&optional n)
  (interactive)
  (tsumego-previous-row (if n (- n) -1)))

(defconst  tsumego-overwrite-mode-list   '(nil ""  t "Overwrite" variation "Variation")
  "first element MUST be nil")

(defun tsumego-toggle-overwrite ()
  (interactive)
  (setq tsumego-overwrite-move (third (memq tsumego-overwrite-move tsumego-overwrite-mode-list)))
  (tsumego-show-status))

(defun tsumego-show-status ()
  (interactive)
  (assert (memq tsumego-current-color '(black white)))
  (setq tsumego-current-color-string  (format "%6s" tsumego-current-color))
  (setq tsumego-mouse-mode-string  (format "%6s" (nth tsumego-mouse-mode '("Place" "Move" ))))
  (setq tsumego-overwrite-mode-string (second (memq tsumego-overwrite-move tsumego-overwrite-mode-list)))
  (setq tsumego-board-number-string (format "%3d/%3d" (1+ tsumego-board-number) (tsumego-board-list-length)))
  (force-mode-line-update))

	

(defun tsumego-reset-color ()
  (interactive)
  (setq tsumego-current-color (or (go-game-current-color (tsumego-current-board)) 'black ))
  (tsumego-show-status))

(defun tsumego-change-color ()
  (interactive)
  (assert (memq tsumego-current-color '(black white)))
  (setq tsumego-current-color (if (eq tsumego-current-color 'black) 'white 'black))
  (tsumego-show-status))


(defun tsumego-place-current-color ()
  (interactive)
  (assert (memq tsumego-current-color '(black white)))
  (tsumego-place-stone tsumego-current-color))

(defun tsumego-stone-color-at (pt)
  (loop for (column row) in (list (tsumego-position-at pt))
	for stone = (go-game-board-aref (tsumego-current-board) column row)
	for color = (and (go-stone-p stone) (go-stone-color stone))
	return color))
	
(defun tsumego-place-black ()
  (interactive)
  (case (tsumego-stone-color-at (point))
    ('black (tsumego-remove-stone))
    ('white (tsumego-remove-stone)
	    (tsumego-place-stone 'black))
    (t    (tsumego-place-stone 'black))))

(defun tsumego-place-white ()
  (interactive)
  (case (tsumego-stone-color-at (point))
    ('white (tsumego-remove-stone))
    ('black (tsumego-remove-stone)
	    (tsumego-place-stone 'white))
    (t    (tsumego-place-stone 'white))))

(defun tsumego-place-stone (color)
  (interactive)
  (loop for (column row) in (list (tsumego-position-at (point)))
	do
	(go-game-place-stone (tsumego-current-board) color column row)
	(tsumego-update-board color column row)))

(defun tsumego-remove-stone ()
  (interactive)
  (loop for (column row) in (list (tsumego-position-at (point)))
	do  
	(go-game-remove-stone (tsumego-current-board) column row)
	(tsumego-update-board nil column row)))

(defun tsumego-move ()
  (interactive)
  (loop for (column row) in (list (tsumego-position-at (point)))
	do
	(when (not (go-game-board-aref (tsumego-current-board) column row))
	  (go-game-move-stone (tsumego-current-board) tsumego-current-color column row tsumego-overwrite-move)
	  (tsumego-update-board tsumego-current-color column row)
	  (tsumego-change-color))))

(defun tsumego-capture ()
  (interactive)
  (loop for (column row) in (list (tsumego-position-at (point)))
	do
	(tsumego-message "capturing (%d,%d)" column row)
	(go-game-capture-stone (tsumego-current-board) column row)
	(tsumego-update-board nil column row)))


(defun tsumego-prev-move ()
  (interactive)
  (let* ((old-stone (go-game-prev-move (tsumego-current-board)))
	 (column    (go-stone-column old-stone))
	 (row       (go-stone-row    old-stone))
	 (color     (go-stone-color  old-stone))
	 (captured  (go-stone-captured-p old-stone)))
    (tsumego-update-board (if captured color nil) column row)
    (tsumego-reset-color)))
;    (tsumego-change-color)))

(defun tsumego-next-move ()
  (interactive)
  (let* ((new-stone (go-game-next-move (tsumego-current-board)))
	 (column    (go-stone-column new-stone))
	 (row       (go-stone-row    new-stone))
	 (color     (go-stone-color  new-stone))
	 (captured  (go-stone-captured-p new-stone)))
    (tsumego-update-board (if captured nil color) column row)
    (tsumego-reset-color)))
;    (tsumego-change-color)))

(defun tsumego-first-move ()
  (interactive)
  (go-game-first-move (tsumego-current-board))
  (tsumego-refresh))

(defun tsumego-last-move ()
  (interactive)
  (go-game-last-move (tsumego-current-board))
  (tsumego-refresh))


(defun tsumego-view-position ()
  (interactive)
  (loop for (column row) in (list (tsumego-position-at (point)))
	do (tsumego-message "(%d,%d)" column row)))

(defun tsumego-toggle-display-image (&optional flag)
  (interactive)
  (if (integerp flag)
      (setq tsumego-display-image-flag (> flag 0))
    (setq tsumego-display-image-flag (not tsumego-display-image-flag)))
  (tsumego-refresh-board (first tsumego-board-coord) (second tsumego-board-coord)
			 'tsumego-board-property))
			   

(defun tsumego-new-board ()
  (interactive)
  (tsumego-board-list-last)
  (tsumego-board-list-insert-after (make-go-game))
  (tsumego-reset-current-board))
  
(defun tsumego-next-board ()
  (interactive)
  (or (tsumego-board-list-next) (tsumego-board-list-first))
  (tsumego-first-move)
  (tsumego-reset-current-board))

(defun tsumego-prev-board ()
  (interactive)
  (or (tsumego-board-list-prev) (tsumego-board-list-last))
  (tsumego-first-move)
  (tsumego-reset-current-board))

(defun tsumego-goto-board (n)
  (interactive "n")
  (and (tsumego-board-list-nth (1- n))
       (tsumego-first-move)
       (tsumego-reset-current-board)))

;; End command definitions


;; Images and Properties
(defun tsumego-position-at ( pos)
  (list (get-text-property  pos 'tsumego-column)
	(get-text-property  pos 'tsumego-row)))
(defun tsumego-color-at ( pos)
  (get-text-property  pos 'tsumego-color))


(defun tsumego-get-image (color column row &optional stone-property)
  (let* ((color-type  (second (assq color `((black bmoku) (white wmoku) (nil empty)))))
	 (place (case row
		  (1  (case column (1 1) (19 3) (t 2)))
		  (19 (case column (1 7) (19 9) (t 8)))
		  (t  (case column (1 4) (19 6) (t 5)))))
	 (type (if (and (eq color-type 'empty) (memq column '(4 10 16)) (memq row '(4 10 16)))
		   'hoshi color-type))
	 (hash-key (list place type (% column 2)))
	 (original-image (cdr (assoc (cons type place) gnugo-xpms))))
    (assert original-image)
    (unless (gethash hash-key tsumego-image-cache)
      (puthash hash-key
	       (create-image 
		(plist-get (cdr original-image) :data) 'xpm t :ascent 'center)
	       tsumego-image-cache))
    (gethash hash-key tsumego-image-cache)))
    

(defun tsumego-board-property (color column row &optional stone-property)
  (list 'tsumego-column column 'tsumego-row row 'tsumego-color color))

(defun tsumego-board-origin ()
  (let ((origin (next-single-property-change (point-min) 'tsumego-column)))
    (assert (equal  (tsumego-position-at origin ) '(1 1)))
    origin))

(defun tsumego-goto-pos (column row)
  (let ((origin  (tsumego-board-origin)))
    (assert (and origin (equal (tsumego-position-at origin) '(1 1))))
    (goto-char (text-property-any (point-min) (point-max) 'tsumego-row    row))
    (goto-char (text-property-any (point)     (point-max) 'tsumego-column column))
    (assert (and origin (equal (tsumego-position-at (point)) (list column row))))
    ))

(defun tsumego-pos (column row)
  "Return buffer position that corresponds to given board coordinate."
  (let* ((origin  (tsumego-board-origin))
	 (line-pos (text-property-any (point-min) (point-max) 'tsumego-row    row))
	 (col-pos  (text-property-any line-pos    (point-max) 'tsumego-column column)))
    (assert (and origin (equal (tsumego-position-at origin) '(1 1))) t )
    (assert (and col-pos (equal (tsumego-position-at col-pos) (list column row))) t)
    col-pos))
(defun tsumego-pos-range (column row)
  (let ((start (tsumego-pos column row)))
    (list start (and start (or (next-property-change start) (point-max) )))))

(defun tsumego-set-property-at-point (props)
  (let ((inhibit-read-only   t))
    (set-text-properties (point) (next-property-change (point)) props)
    (when (not tsumego-display-image-flag)
      (tsumego-refresh))
    ))

(defun tsumego-set-property-at (column row props)
  (let ((inhibit-read-only   t)
	(point (tsumego-pos column row)))
    (assert (and point  (equal (tsumego-position-at point) (list column row))))
    (set-text-properties point (next-property-change point) props)
    (when (not tsumego-display-image-flag)
      (tsumego-refresh))
    ))

      

      


(defun tsumego-dump-xpm ()
  (let ((inhibit-read-only t))
    (mapc (lambda (x)
	    (let ((key (car x))
		  (image (cdr x)))
	      (insert (format "%s\t." key))
	      (put-text-property (+ (point) -1) (point) 'display image)
	      (insert "\n")))
	  gnugo-xpms)
    ))



(defun tsumego-refresh ()
  (interactive)
  (tsumego-refresh-board (car tsumego-board-coord) (cadr tsumego-board-coord) 'tsumego-board-property))



(defun tsumego-refresh-board (x y &optional prop-func)
  (let ((inhibit-read-only t)
	(buffer-column (tsumego-current-column))
	(buffer-line   (tsumego-current-line))
	(current-point (point)))
    (erase-buffer)
    (go-game-print-board (tsumego-current-board) x y prop-func)
    (goto-char current-point)
    (when tsumego-display-image-flag
      (tsumego-display-image))
    (tsumego-show-status)
    ))
    



(defsubst my-add-overlay-properties (overlay properties)
  (loop for (prop val) on properties by 'cddr
	do (overlay-put overlay prop val)
	return  overlay))
(defsubst my-delete-all-overlays-in (beg end)
  (mapc 'delete-overlay (overlays-in beg end)))
(defsubst my-make-overlay (beg end props)
  (let ((overlay (make-overlay beg end (current-buffer))))
    (my-add-overlay-properties overlay props)))


(defvar-local tsumego-overlay-matrix (make-my-matrix 19 19 nil))
(defun tsumego-reset-overlay ()
  (mfill tsumego-overlay-matrix nil))

(defsubst tsumego-put-image (color column row &optional beg end)
    (let* ((pos  (if (and beg end) (list beg end) (tsumego-pos-range column row))))
      (tsumego-set-image-to-overlay (first pos) (second pos) color column row)))

(defun tsumego-set-image-to-overlay (beg end color column row)
  (let* ((props (list 'display (tsumego-get-image color column row) 
		      'board-color color 'board-column column 'board-row row ))
	 (ov (or (mref tsumego-overlay-matrix column row)
		 (mset tsumego-overlay-matrix column row (my-make-overlay beg end props))))
	 (board-column (overlay-get ov 'board-column))
	 (board-row    (overlay-get ov 'board-row))
	 (board-color  (overlay-get ov 'board-color)))
    (if (not (and (eq beg (overlay-start ov)) (eq end (overlay-end ov))))
	(move-overlay ov beg end))
    (unless (and board-column board-row (eq board-color color))
      (my-add-overlay-properties ov props))))
    
    





(defun tsumego-display-image ()
  (loop for pos    = (next-property-change (point-min)) then next-pos  while pos
	for next-pos = (next-property-change pos)
	for color  = (get-text-property pos 'tsumego-color)
	for column = (get-text-property pos 'tsumego-column)
	for row    = (get-text-property pos 'tsumego-row)
	do 
	(when (and column row)
	  (tsumego-put-image color column row pos (or next-pos (point-max))))))

(defun tsumego-update-board (color column row)
  (if (not tsumego-display-image-flag)
      (tsumego-refresh)
    (tsumego-put-image color column row)))



;; board-list management
(defun tsumego-board-list-init ()
  (setq tsumego-board-list (dll-create))
  (dll-enter-last tsumego-board-list (make-go-game))
  (setq tsumego-board-list-iterator  (dll-nth tsumego-board-list 0))
  (setq tsumego-board-number 0)
  (assert (tsumego-board-list-valid-p)))

(defun tsumego-board-list-init-from-sgf (sgf-list)
  (setq tsumego-board-list (dll-create-from-list (sgf-to-go-game-list sgf-list)))
  (let ((n 1))
    (dll-map (lambda (go) (setf (go-game-title go) (format "%s%d" tsumego-title-prefix n)) (incf n)) tsumego-board-list))
  (setq tsumego-board-list-iterator  (dll-nth tsumego-board-list 0))
  (setq tsumego-board-number 0)
  (assert (tsumego-board-list-valid-p)))
  
(defun tsumego-board-list-valid-p()
  (and (dll-p tsumego-board-list) 
       (not (dll-empty tsumego-board-list))
       (go-game-p (dll-element tsumego-board-list tsumego-board-list-iterator))))

(defun tsumego-board-list-nth (n)
  (assert (tsumego-board-list-valid-p))
  (let ((nth-node (dll-nth tsumego-board-list n)))
    (and nth-node
	 (if (< n 0)
	     (setq tsumego-board-number (+ (tsumego-board-list-length) n))
	   (setq tsumego-board-number n))
	 (setq tsumego-board-list-iterator nth-node))))

(defun tsumego-board-list-next ()
  (assert (tsumego-board-list-valid-p))
  (let ((next   (dll-next tsumego-board-list tsumego-board-list-iterator)))
    (and next (incf tsumego-board-number)  (setq tsumego-board-list-iterator  next))))

(defun tsumego-board-list-prev ()
  (assert (tsumego-board-list-valid-p))
  (let ((prev   (dll-previous tsumego-board-list tsumego-board-list-iterator)))
    (and prev (decf tsumego-board-number) (setq tsumego-board-list-iterator  prev))))


(defun tsumego-board-list-first ()
  (tsumego-board-list-nth 0))
(defun tsumego-board-list-last ()
  (tsumego-board-list-nth -1))


(defun tsumego-board-list-insert-after (&optional go)
  (assert (tsumego-board-list-valid-p))
  (dll-enter-after tsumego-board-list tsumego-board-list-iterator (or go (make-go-game)))
  (tsumego-board-list-next)
  (assert (tsumego-board-list-valid-p)))

(defun tsumego-board-list-insert-before (&optional go)
  (assert (tsumego-board-list-valid-p))
  (dll-enter-before tsumego-board-list tsumego-board-list-iterator (or go (make-go-game)))
  (tsumego-board-list-prev)
  (assert (tsumego-board-list-valid-p)))

(defun tsumego-board-list-length ()
  (if (dll-p tsumego-board-list)
      (dll-length tsumego-board-list)
    0))
(defun tsumego-current-board ()
  (assert (tsumego-board-list-valid-p))
  (dll-element tsumego-board-list tsumego-board-list-iterator))

;; END board-list management  

;; IO routines
(defun tsumego-board-list-init-from-file (filename)
  (interactive "FRead a SGF file: ")
  (unless (file-readable-p filename)
    (error "cannot read file:%s" filename))
  
  (let (sgf-game-list)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (setq sgf-game-list (sgf-collection)))
    (tsumego-board-list-init-from-sgf sgf-game-list)))



(defun tsumego-write-sgf-file (filename &optional force)
  (interactive "FWrite all tsumego data as SGF file: ")
  (when (and (not force) (file-exists-p filename)
	     (not (y-or-n-p "File exists. Continue? ")))
    (error "Not writing %s" filename))
  (if  (not (tsumego-board-list-valid-p))
      (message "error in tsumego data,not writing to file")
    (let ((game-list tsumego-board-list))
      (with-temp-buffer
	(dll-map 
	 (lambda (go)  (unless (go-game-empty-p go) 
			 (insert (go-game-tree-to-sgf-string go) "\n")))
	 game-list)
	(tsumego-message "writing to %s" filename)
	(write-file filename)))))

(defun tsumego-write-to-default-sgf-file ()
  (interactive)
  (tsumego-write-sgf-file tsumego-sgf-default-filename t))

	   
(defun tsumego-mode ()
  "Major mode to edit sgf file"
  (interactive)
  (kill-all-local-variables)
  (tsumego-change-mouse-mode 0)
  (setq major-mode 'tsumego-mode)
  (setq mode-name "Tsumego")
  (setq buffer-read-only t)
  (setq truncate-lines t)

  (setq mode-line-format 
	(list "-"
	      'mode-line-mule-info
	      'mode-line-modified
	      'mode-line-frame-identification
	      'mode-line-buffer-identification
	      'global-mode-string
	      "  <Tsumego Edit>  " 'tsumego-board-number-string " "
	      " Color:" 'tsumego-current-color-string
	      " Mode: " 'tsumego-mouse-mode-string
	      "\t"     'tsumego-overwrite-mode-string
	      
	      ))
  (add-hook 'kill-buffer-hook 'tsumego-kill-buffer nil t)
  (run-hooks 'tsumego-mode-hook))

(defun tsumego-kill-buffer ()
  (when tsumego-idle-timer
    (cancel-timer tsumego-idle-timer))
  (run-hooks 'tsumego-exit-hook))

(defun tsumego-auto-save(buffer)
  (with-current-buffer buffer
    (when tsumego-need-auto-save
      (setq tsumego-need-auto-save nil)
      (message "auto saving to %s ..." tsumego-sgf-auto-save-filename)
      (tsumego-write-sgf-file tsumego-sgf-auto-save-filename t))))

(defun tsumego-reset-current-board ()
  (setq tsumego-state 'placing)
  (tsumego-refresh)
  (tsumego-goto-pos (first tsumego-initial-position) (second tsumego-initial-position))
  (tsumego-reset-color)
  (tsumego-change-mouse-mode 0)
  )

(defun tsumego (filename)
  (interactive "FSGF file name to edit in this session: ")
  (switch-to-buffer "*tsumego*")
  (tsumego-mode)
  (setq tsumego-sgf-default-filename filename)
  (setq tsumego-sgf-auto-save-filename (concat filename ".bak"))

  (if (file-exists-p filename) 
      (progn 
	(tsumego-board-list-init-from-file filename)
	(setq tsumego-idle-timer 
	      (when (numberp tsumego-auto-save-idle-time)
		(run-with-idle-timer tsumego-auto-save-idle-time t 'tsumego-auto-save (current-buffer))))
	)
    (message "file %s does not exist,editing from scratch" filename)
    (tsumego-board-list-init))
  
  (tsumego-reset-overlay)
  (tsumego-reset-current-board)
  )

(provide 'tsumego)
