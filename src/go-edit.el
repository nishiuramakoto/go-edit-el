
;; See elisp manual
;(require 'cl)
(eval-when-compile (require 'cl))

(require 'mylib)

(require 'goban)
(require 'go-game)
(require 'go-clock)
(require 'dll) ;; for doubly linked list


;; user variable section
(defvar go-edit-display-image-flag nil)
(defvar go-edit-overwrite-mode nil)
(defvar go-edit-mode-hook nil)
(defvar go-edit-exit-hook nil)
(defvar go-edit-show-file-info t)
(defvar go-edit-default-mouse-mode 0)
(defvar go-edit-initial-position '(4 4))
(defvar go-edit-auto-save-idle-time nil
  "An interger or floating point value causes auto saving in that interval")
(defvar go-edit-verbose-message nil)
(defvar go-edit-name-prefix "")
(defvar go-edit-error-threshold 5)
(defvar go-edit-cursor-type 'box)
(defvar go-edit-main-window-size 118)
(defvar go-edit-preserve-comment nil)
(defvar go-edit-always-inherit-label nil)
(defvar go-edit-transform-matrix '(1 0 0 1))
(defvar go-edit-buffer-name      "*go edit*")
(defvar go-edit-info-buffer-name "*go edit info*")
(defvar go-edit-info-tab-width  20)
(defvar go-edit-memorize-mode nil)

;; end user variable section

(defvar go-edit-window-configuration)
(defvar go-edit-main-window)
(defvar go-edit-main-buffer)
(defvar go-edit-info-window)
(defvar go-edit-info-buffer)
(defvar go-edit-current-sgf-file)

;; interval buffer local variables


(defvar-local go-edit-game-list )
(defvar-local go-edit-game-list-iterator)
(defvar-local go-edit-game-number 0)
(defvar-local go-edit-game-number-string)
(defvar-local go-edit-goban)
(defvar-local go-edit-current-game)
(defvar-local go-edit-sgf-default-filename)
(defvar-local go-edit-sgf-auto-save-filename)
(defvar-local go-edit-current-color-string)
(defvar-local go-edit-overwrite-mode-string)
(defvar-local go-edit-mouse-mode-string)
(defvar-local go-edit-idle-timer)
(defvar-local go-edit-mouse-prev-error-count 0)
(defvar-local go-edit-mouse-next-error-count 0)
(defvar-local go-edit-move-error-count 0)
(defvar-local go-edit-need-auto-save t)
(defvar-local go-edit-num-mistakes 0)
(defvar-local go-edit-timer nil)

(defvar go-edit-mode-keyboard-map
  (let ((map  (make-sparse-keymap)))
    (define-key map "h"        	'backward-char)
    (define-key map "j"    	'next-line)
    (define-key map "k"    	'previous-line)
    (define-key map "l"    	'forward-char)
;    (define-key map "i"      	'go-edit-toggle-display-image)
    (define-key map "\C-d"   	'go-edit-remove-stone)
    (define-key map "\M-\C-d"   'go-edit-delete-game)
;    (define-key map "p"         'go-edit-pass-move)
    (define-key map "T"      	'go-edit-change-color)
    (define-key map "^"      	'go-edit-toggle-triangle)
    (define-key map "o"      	'go-edit-toggle-circle)
    (define-key map "["      	'go-edit-toggle-square)
    (define-key map "x"      	'go-edit-toggle-mark)
    (define-key map "/"      	'go-edit-set-label)
    (define-key map "L"      	'go-edit-inherit-label)
    (define-key map "="      	'go-edit-view-position)
    (define-key map "r"      	'go-edit-refresh)
    (define-key map "R"      	'go-edit-reset-memorize)
    (define-key map "I"      	'go-edit-show-info)
    (define-key map "i"      	'go-edit-show-info-except-result)
    (define-key map "Q"      	'go-edit-set-problem-how-much)
    (define-key map "s"      	'go-edit-write-sgf-file)
    (define-key map "S"      	'go-edit-write-to-default-sgf-file)
    (define-key map "g"      	'go-edit-goto-game)
    (define-key map "b"      	'go-edit-place-black)
    (define-key map "c"      	'go-edit-set-comment)
    (define-key map "w"      	'go-edit-place-white)
    (define-key map "."      	'go-edit-move)
    (define-key map "n"      	'go-edit-show-next-moves)
    (define-key map "?"      	'go-edit-what-stone)
    (define-key map "!"      	'go-edit-new-game-with-current-position)
    (define-key map [insert] 	'go-edit-toggle-overwrite)
    (define-key map "\C-m"   	'go-edit-new-game)
    (define-key map [prior]  	'go-edit-prev-game)
    (define-key map [next]  	'go-edit-next-game)
    (define-key map [home]  	'go-edit-first-move)
    (define-key map [end]  	'go-edit-last-move)
    (define-key map [right]  	'go-edit-next-move)
    (define-key map [left]  	'go-edit-prev-move)
    (define-key map [delete]  	'go-edit-delete-move)
    (define-key map [C-delete]  'go-edit-delete-variation)
    (define-key map [S-down]    'go-edit-next-variation)
    (define-key map [S-up]      'go-edit-prev-variation)
    (define-key map [S-right]   'go-edit-next-move)
    (define-key map [S-left]    'go-edit-prev-move)

    (loop for key from 1 to 9
	  for coord = (nth key '(nil (4 16) (10 16) (16 16) (4 10) (10 10) (16 10) (4 4) (10 4) (16 4)))
	  do  
	  (lexical-let ((coord coord))
	    (define-key map  (format "%d" key) (lambda () (interactive)
						 (goto-char (apply 'goban-position go-edit-goban coord))))))
	  
    map)
  "keymap to edit go-edit.")


  
;; Mouse commands
(defmacro go-edit-mouse-generic-command (command)
  "Return a function that sets point where a mouse event has happened,and do COMMAND. "
  `(lambda (e)
     (interactive "@e")
     (unwind-protect
	 (progn
	   (go-edit-message "mouse event:%s" e)
	   (mouse-set-point e)
	   (call-interactively ,command))
       (message "test")
       (goto-char (point-min))
       (scroll-down)
       (sit-for 1))
     ))
       
     


(defvar go-edit-mode-mouse-map-list
  (loop for map in
	(list 
	 (let ((map  (make-sparse-keymap)))
	   (define-key map [down-mouse-1] (go-edit-mouse-generic-command 'go-edit-place-black))
	   (define-key map [down-mouse-3] (go-edit-mouse-generic-command 'go-edit-place-white))
	   (define-key map [mouse-3] 'undefined)
	   (define-key map [down-mouse-4] 'go-edit-mouse-prev)
	   (define-key map [down-mouse-5] 'go-edit-mouse-next)

	   map)
	 (let ((map  (make-sparse-keymap)))
;	   (define-key map [down-mouse-1]         (go-edit-mouse-generic-command 'go-edit-move))
	   (define-key map [down-mouse-1]         (go-edit-mouse-generic-command 'go-edit-move-or-next))
	   (define-key map [down-mouse-3]         (go-edit-mouse-generic-command 'go-edit-capture))
	   (define-key map [double-down-mouse-3]  'go-edit-change-color)
	   (define-key map [down-mouse-4] 'go-edit-mouse-prev)
	   (define-key map [down-mouse-5] 'go-edit-mouse-next)
	   (define-key map [mouse-4] 'undefined)
	   (define-key map [mouse-5] 'undefined)
	   map)
	 )
	do
	(define-key map [mouse-2] 'go-edit-change-mouse-mode)
	(define-key map [mouse-1] 'go-edit-undefined-command)
	(define-key map [mouse-3] 'go-edit-undefined-command)
	(define-key map [mouse-4] 'go-edit-undefined-command)
	(define-key map [mouse-5] 'go-edit-undefined-command)
	(set-keymap-parent map go-edit-mode-keyboard-map)
	collect map))

  
(defun go-edit-undefined-command (&rest arg)
  "This function is to prevent unwanted point or mark change \n
that results in very annoying image scrolling behavior not seen in previous versions of emacs.\n
I will continue to use this until I find the exact reason.
"
  (interactive)
  )

(assert (not (memq nil (mapcar (lambda (map) (eq (keymap-parent map) go-edit-mode-keyboard-map))
			       go-edit-mode-mouse-map-list))))
	       
(defvar go-edit-mouse-mode 0)
(make-variable-buffer-local 'go-edit-mouse-mode)

(defun go-edit-change-mouse-mode (&optional mode)
  (interactive)
  (let ((inhibit-read-only t))
    (if mode
	(setq go-edit-mouse-mode (% mode (length go-edit-mode-mouse-map-list)))
      (setq go-edit-mouse-mode (% (+ go-edit-mouse-mode 1) (length go-edit-mode-mouse-map-list))))
    (use-local-map (nth go-edit-mouse-mode go-edit-mode-mouse-map-list))
    (go-edit-show-status)))
(defun go-edit-reset-mouse-mode ()
  (interactive)
  (go-edit-change-mouse-mode 0))


(defun go-edit-message (&rest args)
  (when go-edit-verbose-message
    (apply 'message args)))

;; Command definitions

(defmacro defun-go-edit-threshold-func (func-name counter-name usual-func emergency-func)
  `(defun ,func-name ()
     ,(format "Usually just execute %s,but too many successive errors cause to call %s.
Note that this function is automatically generated by the macro defun-go-edit-threshold-func." usual-func  emergency-func)
     (interactive)
     (condition-case sig
	 (progn
	   (,usual-func)
	   (setf ,counter-name 0))
       (error 
	(message "signal:%s" sig)
	(incf ,counter-name)
	(when (>  ,counter-name  go-edit-error-threshold)
	  (,emergency-func)
	  (setf ,counter-name 0))))))


(defun-go-edit-threshold-func go-edit-mouse-prev   go-edit-mouse-prev-error-count go-edit-prev-move go-edit-prev-game)
(defun-go-edit-threshold-func go-edit-mouse-next   go-edit-mouse-next-error-count go-edit-next-move go-edit-next-game)
(defun-go-edit-threshold-func go-edit-move-or-next go-edit-move-error-count       go-edit-move      go-edit-next-move)


(defconst  go-edit-overwrite-mode-list   '(nil ""  t "Overwrite" variation "Variation")
  "first element MUST be nil")

(defvar go-edit-autoplay-timer nil)
(defvar go-edit-autoplay-interval 10)

(defun go-edit-toggle-autoplay ()
  (interactive)
  (if go-edit-autoplay-timer
      (go-edit-cancel-autoplay-timer)
    (setq go-edit-autoplay-timer (run-at-time t go-edit-autoplay-interval
					      (lambda () (if (go-edit-at-last-node)
							     (go-edit-cancel-autoplay-timer)
							   (go-edit-next-move)))))))
(defun go-edit-cancel-autoplay-timer ()
  (cancel-timer go-edit-autoplay-timer)
  (setq go-edit-autoplay-timer nil))

(defun go-edit-reset-memorize ()
  (interactive)
  (go-game-first-move go-edit-current-game)
  (go-edit-reset-current-game))

(defun go-edit-at-last-node ()
  (go-game-tree-at-last-node go-edit-current-game))

(defun go-edit-toggle-overwrite ()
  (interactive)
  (setq go-edit-overwrite-mode (third (memq go-edit-overwrite-mode go-edit-overwrite-mode-list)))
  (go-edit-show-status))

(defun go-edit-show-status ()
  (interactive)
  (setq go-edit-current-color-string  (format "%6s" (go-edit-current-color)))
  (setq go-edit-mouse-mode-string  (format "%6s" (nth go-edit-mouse-mode '("Place" "Move" ))))
  (setq go-edit-overwrite-mode-string (second (memq go-edit-overwrite-mode go-edit-overwrite-mode-list)))
  (setq go-edit-game-number-string (format "%3d/%3d" (1+ go-edit-game-number) (go-edit-game-list-length)))
  (force-mode-line-update))

(defun go-edit-set-problem-how-much (yose-type value)
  (interactive "cYose type (s:sente g:gote r:reverse sente)? \nsHow much? ")
  (let ((point (string-to-number value))
	(type  (case yose-type (?s 'sente) (?g 'gote) (?r 'reverse-sente) (t (error "invalid yose type")))))
  (go-game-set-problem-how-much go-edit-current-game (go-edit-current-color)  type point nil go-edit-transform-matrix)
  (go-game-last-move go-edit-current-game)
  (go-edit-refresh)))
	
(defun go-edit-clear-all-labels ()
  (interactive)
  "Clear all labels of current node. Labels of ancestor nodes are not affected."
  (go-game-clear-all-labels go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-current-color ()
  (if  (go-game-p go-edit-current-game) 
      (go-game-player-color go-edit-current-game)
    "undef"))
 (defun go-edit-change-color ()
   (interactive)
   (go-game-change-color go-edit-current-game)
   (go-edit-update-display))

(defun go-edit-inherit-label ()
  (interactive)
  (go-game-inherit-label go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-coord-at (&optional p)
  (goban-coord-at go-edit-goban p))
(defun go-edit-column-at (&optional p)
  (goban-column-at go-edit-goban p))
(defun go-edit-row-at (&optional p)
  (goban-row-at go-edit-goban p))


(defun go-edit-stone-color-at (&optional p)
  (go-game-board-ref-color go-edit-current-game (go-edit-column-at) (go-edit-row-at)))

(defun go-edit-stone-property-at (&optional point)
  (go-game-board-ref-property go-edit-current-game (go-edit-column-at) (go-edit-row-at)))

(defun go-edit-place-black ()
  (interactive)
  (case (go-edit-stone-color-at (point))
    ('black (go-edit-remove-stone))
    ('white (go-edit-remove-stone)
	    (go-edit-place-stone 'black))
    (t    (go-edit-place-stone 'black))))

(defun go-edit-place-white ()
  (interactive)
  (case (go-edit-stone-color-at (point))
    ('white (go-edit-remove-stone))
    ('black (go-edit-remove-stone)
	    (go-edit-place-stone 'white))
    (t    (go-edit-place-stone 'white))))

(defun go-edit-place-stone (color)
  (interactive)
  (go-game-place-stone go-edit-current-game color (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-remove-stone ()
  (interactive)
  (go-game-remove-stone go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-remove-stone-all ()
  (interactive)
  (go-game-remove-stone-all go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-delete-move ()
  (interactive)
  (let ((go go-edit-current-game))
    (if (go-game-tree-at-root-node go)
	(go-game-remove-stone-all go)
      (go-game-delete-move go))
    (go-edit-update-display)))

(defun go-edit-move ()
  (interactive)
  (condition-case error-info
      (go-game-move-stone go-edit-current-game (go-edit-column-at) (go-edit-row-at) go-edit-overwrite-mode)
    (error
     (incf go-edit-num-mistakes)
     (error "go-edit-move:no such move %s" error-info)))
  (when go-edit-always-inherit-label
    (go-edit-inherit-label))
  (go-edit-update-display))

(defun go-edit-capture ()
  (interactive)
  (go-game-capture-stone go-edit-current-game  (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-prev-move ()
  (interactive)
  (go-game-prev-move go-edit-current-game)
  (go-edit-update-display))


(defun go-edit-next-move ()
  (interactive)
  (go-game-next-move go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-first-move ()
  (interactive)
  (go-game-first-move go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-last-move ()
  (interactive)
  (go-game-last-move go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-show-candidate-moves ()
  (interactive)
  (message "next moves=%s" (go-game-next-move-list go-edit-current-game)))

(defun go-edit-new-game ()
  (interactive)
  (go-edit-game-list-last)
  (go-edit-game-list-insert-after (make-go-game))
  (go-edit-reset-current-game)
  (go-edit-reset-mouse-mode)
  )

(defun go-edit-new-game-with-current-position ()
  (interactive)
  (let ((new-game (make-go-game go-edit-current-game)))
    (go-edit-game-list-insert-after new-game)
    (go-edit-reset-current-game)))

(defun go-edit-delete-game ()
  (interactive)
  (go-edit-game-list-delete)
  (go-edit-reset-current-game))
  
(defun go-edit-next-game ()
  (interactive)
  (or (go-edit-game-list-next) (go-edit-game-list-first))
  (go-edit-first-move)
  (go-edit-reset-current-game))

(defun go-edit-prev-game ()
  (interactive)
  (or (go-edit-game-list-prev) (go-edit-game-list-last))
  (go-edit-first-move)
  (go-edit-reset-current-game))

(defun go-edit-goto-game (n)
  (interactive "n")
  (and (go-edit-game-list-nth (1- n))
       (go-edit-first-move)
       (go-edit-reset-current-game)))


(defvar-local go-edit-buffer nil
  "This variables is needed becase of lack of support for closures in Emacs Lisp")


(defun go-edit-what-stone ()
  (interactive)
  (let ((stone (go-game-board-aref go-edit-current-game  (go-edit-column-at) (go-edit-row-at))))
    (message "what stone:%s" stone)))

(defun go-edit-toggle-triangle ()
  (interactive)
  (go-game-toggle-triangle go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-toggle-circle ()
  (interactive)
  (go-game-toggle-circle go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-toggle-square ()
  (interactive)
  (go-game-toggle-square go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-toggle-mark ()
  (interactive)
  (go-game-toggle-mark go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-set-label (&optional label)
  (interactive "sEnter label:")
  (go-game-set-label go-edit-current-game (go-edit-column-at) (go-edit-row-at) label)
  (go-edit-update-display))

(defun go-edit-delete-markup ()
  (interactive)
  (go-game-delete-markup  go-edit-current-game (go-edit-column-at) (go-edit-row-at))
  (go-edit-update-display))

(defun go-edit-set-comment (&optional comment)
  (interactive "sEnter comment:")
  (go-game-set-comment go-edit-current-game comment)
  (go-edit-show-comment))

(defun go-edit-next-variation ()
  (interactive)
  (go-game-next-variation go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-prev-variation ()
  (interactive)
  (go-game-prev-variation go-edit-current-game)
  (go-edit-update-display))


(defun go-edit-refresh ()
  (interactive)
  (goban-refresh go-edit-goban go-edit-current-game)
  (go-edit-update-display))

(defun go-edit-toggle-cursor ()
  (interactive)
  (setq cursor-type (not cursor-type)))

;; End command definitions


(defun go-edit-dump-xpm ()
  (let ((inhibit-read-only t))
    (mapc (lambda (x)
	    (let ((key (car x))
		  (image (cdr x)))
	      (insert (format "%s\t." key))
	      (put-text-property (+ (point) -1) (point) 'display image)
	      (insert "\n")))
	  gnugo-xpms)
    ))


;; game-list management
(defun go-edit-game-list-init ()
  (setq go-edit-game-list (dll-create))
  (dll-enter-last go-edit-game-list (make-go-game))
;  (setq go-edit-game-list-iterator  (dll-nth go-edit-game-list 0))
  (go-edit-set-game (dll-nth go-edit-game-list 0))
  (setq go-edit-game-number 0)
  (assert (go-edit-game-list-valid-p)))

(defun go-edit-game-list-init-from-file (file)
  (let ((game-list (make-go-game-list-from-file file)))
    (when game-list
      (setq go-edit-game-list (dll-create-from-list game-list))
      (go-edit-set-game (dll-nth go-edit-game-list 0))
      (setq go-edit-game-number 0)
      (assert (go-edit-game-list-valid-p)))))
  
(defun go-edit-game-list-valid-p()
  (and (dll-p go-edit-game-list) 
       (not (dll-empty go-edit-game-list))
       (go-game-p (dll-element go-edit-game-list go-edit-game-list-iterator))))

(defun go-edit-game-list-nth (n)
  (assert (go-edit-game-list-valid-p))
  (let ((nth-node (dll-nth go-edit-game-list n)))
    (and nth-node
	 (if (< n 0)
	     (setq go-edit-game-number (+ (go-edit-game-list-length) n))
	   (setq go-edit-game-number n))
	 (go-edit-set-game  nth-node))))

(defun go-edit-game-list-next ()
  (assert (go-edit-game-list-valid-p))
  (let ((next   (dll-next go-edit-game-list go-edit-game-list-iterator)))
    (and next (incf go-edit-game-number)  (go-edit-set-game next))))

(defun go-edit-game-list-prev ()
  (assert (go-edit-game-list-valid-p))
  (let ((prev   (dll-previous go-edit-game-list go-edit-game-list-iterator)))
    (and prev (decf go-edit-game-number) (go-edit-set-game  prev))))
(defun go-edit-game-list-delete ()
  (assert (go-edit-game-list-valid-p))
  (let ((current-node go-edit-game-list-iterator))
    (go-edit-game-list-next)
    (dll-delete go-edit-game-list current-node)))

(defun go-edit-game-list-first ()
  (go-edit-game-list-nth 0))
(defun go-edit-game-list-last ()
  (go-edit-game-list-nth -1))


(defun go-edit-game-list-insert-after (&optional go)
  (assert (go-edit-game-list-valid-p))
  (dll-enter-after go-edit-game-list go-edit-game-list-iterator (or go (make-go-game)))
  (go-edit-game-list-next)
  (assert (go-edit-game-list-valid-p)))

(defun go-edit-game-list-insert-before (&optional go)
  (assert (go-edit-game-list-valid-p))
  (dll-enter-before go-edit-game-list go-edit-game-list-iterator (or go (make-go-game)))
  (go-edit-game-list-prev)
  (assert (go-edit-game-list-valid-p)))

(defun go-edit-game-list-length ()
  (if (dll-p go-edit-game-list)
      (dll-length go-edit-game-list)
    0))

(defun go-edit-set-game (iterator)

  (setq go-edit-game-list-iterator iterator)
  (setq go-edit-current-game (dll-element go-edit-game-list  iterator))
  (assert (go-game-p go-edit-current-game) t)
  (goban-refresh go-edit-goban go-edit-current-game))


;; END game-list management  

;; IO routines
(defun go-edit-read-sgf-file  (filename)
  (interactive "FRead a SGF file: ")
  (unless (file-readable-p filename)
    (error "cannot read file:%s" filename))
  (go-edit-game-list-init-from-file filename))



(defun go-edit-write-sgf-file (filename &optional force)
  (interactive "FWrite all go-edit data as SGF file: ")
  (when (and (not force) (file-exists-p filename)
	     (not (y-or-n-p "File exists. Continue? ")))
    (error "Not writing %s" filename))
  (if  (not (go-edit-game-list-valid-p))
      (message "error in go-edit data,not writing to file")
    (let ((game-list go-edit-game-list))
      (with-temp-buffer
	(dll-map 
	 (lambda (go)  (unless (go-game-empty-p go) 
			 (insert (go-game-to-sgf-string go) "\n")))
	 game-list)
	(go-edit-message "writing to %s" filename)
	(write-file filename)))))

(defun go-edit-write-to-default-sgf-file ()
  (interactive)
  (go-edit-write-sgf-file go-edit-sgf-default-filename t))

	   
(defun go-edit-mode ()
  "Major mode to edit sgf file"
  (interactive)
  (kill-all-local-variables)
;  (go-edit-change-mouse-mode 0)
  (go-edit-change-mouse-mode go-edit-default-mouse-mode)
  (setq major-mode 'go-edit-mode)
  (setq mode-name "Go-Edit")
  (setq buffer-read-only t)
  (setq truncate-lines t)

  (setq mode-line-format 
	(list "-"
	      'mode-line-mule-info
	      'mode-line-modified
	      'mode-line-frame-identification
	      'mode-line-buffer-identification
	      'global-mode-string
	      "  <Go Edit>  " 'go-edit-game-number-string " "
	      '(:eval (upcase go-edit-current-color-string)) " to move "
	      " Mode: " 'go-edit-mouse-mode-string
	      "\t"     'go-edit-overwrite-mode-string
	      
	      ))
  (add-hook 'kill-buffer-hook 'go-edit-kill-buffer nil t)
;  (set (make-variable-buffer-local 'cursor-type) go-edit-cursor-type)
  (set (make-local-variable 'cursor-type) go-edit-cursor-type)

  (run-hooks 'go-edit-mode-hook))

(defun go-edit-kill-buffer ()
  (when (window-configuration-p go-edit-window-configuration)
    (set-window-configuration go-edit-window-configuration))
  (kill-buffer go-edit-info-buffer)
  (when go-edit-idle-timer
    (cancel-timer go-edit-idle-timer))
  (run-hooks 'go-edit-exit-hook))

(defun go-edit-auto-save(buffer)
  (with-current-buffer buffer
    (when go-edit-need-auto-save
      (setq go-edit-need-auto-save nil)
      (message "auto saving to %s ..." go-edit-sgf-auto-save-filename)
      (go-edit-write-sgf-file go-edit-sgf-auto-save-filename t))))

(defun go-edit-reset-current-game ()
  (goto-char (apply 'goban-position go-edit-goban go-edit-initial-position))
  (go-edit-change-mouse-mode go-edit-default-mouse-mode)
  (go-edit-show-info nil)
  (setq go-edit-num-mistakes 0)
  (setq go-edit-timer (make-go-timer))
  (go-edit-update-display))

(defun go-edit-update-display ()
  (goban-update go-edit-goban go-edit-current-game)
  (if go-edit-preserve-comment
      (go-edit-show-text "\n------------------------------\n")
    (go-edit-erase-text))
  (go-edit-show-status)
  (go-edit-show-comment)
  (when go-edit-memorize-mode
    (go-edit-show-text (format "%s mistakes\n%s moves\n%s sec" 
			       go-edit-num-mistakes
			       (go-game-move-number go-edit-current-game)
			       (round (go-timer-elapsed-time-sec go-edit-timer)))))
  (when (< 1 (length (go-game-alternate-move-list go-edit-current-game)))
    (go-edit-show-text "There is a variation"))
  (when (go-game-tree-at-last-node go-edit-current-game)
    (go-edit-show-text "[No more moves]")
    (go-edit-show-info nil)))

(defun go-edit-show-text (string)
  (with-current-buffer go-edit-info-buffer
    (when string
      (insert string))))
(defun go-edit-erase-text ()
  (with-current-buffer go-edit-info-buffer
    (erase-buffer)))


(defun go-edit-show-comment ()
  (go-edit-show-text (go-game-comment go-edit-current-game)))
  
(defun go-edit-show-info (&optional without-result)
  (interactive "P")
  (when go-edit-current-game
    (let* ((go        go-edit-current-game)
	   (game-info (go-game-info go))
	   (file       (expand-file-name go-edit-current-sgf-file)))
      (with-current-buffer go-edit-info-buffer
	(insert "\n------------------------------\n")
	(when (and file go-edit-show-file-info (file-exists-p file))
	  (insert (shell-command-to-string (format "ls -l \"%s\"" file))
		  "\n"))
	(loop for (info-type . info) in game-info
	      if (or (not (equal info-type "result")) (not without-result))
	      do (insert info-type "\t:" info "\n"))
	(insert (format "move number\t:%d\nblack agehama\t:%d\nwhite agehama\t:%d\n" 
			(go-game-move-number go)
			(go-game-agehama-black go) 
			(go-game-agehama-white go)))))))


(defun go-edit-show-info-except-result ()
  (interactive)
  (go-edit-show-info t))


(defun go-edit-set-info-buffer ()
  (when (not (get-buffer go-edit-info-buffer-name))
    (setq go-edit-info-buffer (get-buffer-create go-edit-info-buffer-name))
    (with-current-buffer go-edit-info-buffer
;      (set (make-variable-buffer-local 'truncate-partial-width-windows) nil)
      (set (make-local-variable 'truncate-partial-width-windows) nil)
      (set (make-local-variable 'cursor-type) nil)
      (set (make-local-variable 'tab-width) go-edit-info-tab-width )
      (run-hooks 'go-edit-info-setup-hook))))
  
(defun go-edit-set-window ()
  (interactive)
  (delete-other-windows)
  (setq go-edit-main-window (selected-window))
  (setq go-edit-main-buffer (get-buffer-create go-edit-buffer-name))
  (condition-case error
      (setq go-edit-info-window (split-window-horizontally go-edit-main-window-size))
    (error
     (setq go-edit-info-window (selected-window))
     (message "Could not split the window horizontally.Try either to decrease go-edit-main-window-size or
to widen the window manually. error=\"%s\"" error)))

  (go-edit-set-info-buffer)

  (select-window go-edit-info-window)
  (switch-to-buffer go-edit-info-buffer)

  (select-window go-edit-main-window)
  (switch-to-buffer go-edit-main-buffer)
  )
  
(defun go-edit (filename)
  (interactive "FSGF file name to edit in this session: ")
  (setq go-edit-window-configuration (current-window-configuration))

  (if (get-buffer go-edit-buffer-name)
      (progn
	(go-edit-set-window)
	(message "go edit buffer already exists;just switched to the buffer"))
    (go-edit-set-window)
    (go-edit-mode)
    (setq go-edit-current-sgf-file filename)
    (setq go-edit-goban (make-goban))
    (setq go-edit-sgf-default-filename filename)
    (setq go-edit-sgf-auto-save-filename (concat filename ".bak"))
    (setq go-edit-num-mistakes 0)
    (setq go-edit-timer (make-go-timer))

    (if (file-exists-p filename) 
	(progn 
	  (go-edit-game-list-init-from-file filename)
	  (when (or (not go-edit-current-game) (not (go-game-p go-edit-current-game)))
	    (if (yes-or-no-p (format "File %s does not seem to be a sgf file. Continue ?" filename))
		(go-edit-game-list-init)
	      (go-edit-kill-buffer)
	      (error "Abort editing")))
	  (setq go-edit-idle-timer 
		(when (numberp go-edit-auto-save-idle-time)
		  (run-with-idle-timer go-edit-auto-save-idle-time t 'go-edit-auto-save (current-buffer))))
	  )
      (message "file %s does not exist,editing from scratch" filename)
      (go-edit-game-list-init))
    
    (assert (go-game-p go-edit-current-game))
    (go-edit-reset-current-game)
  ))

(provide 'go-edit)
