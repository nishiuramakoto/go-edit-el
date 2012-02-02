;; -*- mode:lisp-interaction ; -*-

(setq go-edit-log-file (expand-file-name "~/var/go-edit.log"))
(defun go-edit-log (mess &rest obj)
  (with-temp-buffer
    (erase-buffer)
    (goto-char (point-max))
    (insert (apply 'format mess obj))
    (insert "\n")
    (append-to-file (point-min) (point-max) go-edit-log-file)))
  
(setq go-edit-show-file-info nil)
(progn
  (require 'cl)
  (require 'mylib)
  (makunbound 'go-edit-mode-keyboard-map)
  (makunbound 'go-edit-mode-mouse-map-list)
;  (setq go-edit-always-inherit-label t)
  (setq go-edit-cursor-type nil)
;  (setq go-edit-transform-matrix '(0 1 -1 0))
  (setq go-edit-default-mouse-mode 1)
  (setq go-edit-main-window-size 101)
  (setq go-edit-need-auto-save nil)  
  (setq go-game-default-white nil)
  (setq go-edit-memorize-mode t)

  (load-default "sgf")
  (load-default "my-tree")
  (load-default "goban")
  (load-default "go-clock")
  (load-default "go-game")
  (load-default "go-edit")
  (load-default "mylib")
  (defun pickup-random-game (listfile)
    (assert (file-exists-p listfile))
    (with-current-buffer (find-file-noselect listfile)
      (goto-line (+ 1 (random (count-lines (point-min) (point-max)))))
      (buffer-substring (line-beginning-position) (line-end-position))))
  (defun view-sgf (file)
    (interactive)
    (let* ((moves-per-diagram 50)
	   (status (my-call-process "sgf2dg" "-movesPerDiagram" (format "%s" moves-per-diagram)  "-doubleDigits" "-out" "/tmp/tmp.pdf"  "-converter" "PDF" "-i" file)))
	  (assert (= status 0))
	  (my-call-process-async "xpdf" "/tmp/tmp.pdf")))
  (defun dired-view-sgf ()
    (interactive)
    (let* ((file (dired-get-filename)))
      (go-edit-log file)
      (view-sgf file)))
    
    
  (defun dired-go-edit (&optional kifu)
    (interactive "P")
    (let* ((file (dired-get-filename)))
      (when kifu
	(view-sgf file))
      (go-edit-log file)
      (go-edit file)))


  (defun go-edit-with-clock (file)
      (setq timeclock-use-elapsed t)
      (timeclock-modeline-display t)
      (timeclock-in)    
      (go-edit file)
      (timeclock-out))

  (defun go-edit-random-game (gamelist)
    (let ((sgf-file (pickup-random-game gamelist))
	  (default-directory (file-name-directory gamelist)))
      (print sgf-file)
;      (go-edit-with-clock sgf-file)
      ))

  (defun go-edit-random-title-match ()
    (go-edit-random-game "~/go/games/pro/title/gamelist.txt"))
  (defun go-edit-random-chikun ()
    (go-edit-random-game "~/go/games/pro/ChoChikun/gamelist.txt"))
  )

;(go-edit-random-title-match)
;(go-edit-random-chikun)


























;; (with-current-buffer "*go edit*"
;;   (let* ((go go-edit-current-game)
;; 	 (node (go-game-tree-current-node go)))
;;     (sgf-go-node-placed-stone-list  node 'white)
;;     (go-game-remove-stone-all go)
;;   ))



;; (assert (equal (sgf-coordinate "aa") '(1 1)))
;; (assert (equal (sgf-point 1 1) "aa"))
;; (assert (equal (sgf-column "ab") 1))
;; (assert (equal (sgf-row "ab") 2))

;; (let* ((prop (make-sgf-property "B"))
;;        (n    (make-sgf-node (list))))
;;   (sgf-property-add-value prop "ab")
;;   (sgf-property-add-value prop "cd")
;;   (sgf-property-to-string prop)
;;   (sgf-node-add-property n prop)
;;   (sgf-node-search-property n "B" "cd")
;;   (sgf-node-add-property-value n "B" "zz")
;;   (sgf-node-add-property-value n "C" "test")
;;   (sgf-node-delete-property-value n "B" "cd")
;;   (sgf-node-delete-property-value n "B" "zz")
;;   (sgf-node-delete-property-value n "B" "ab")
;;   (sgf-node-add-property-value n "B" "zz")
;;   (sgf-node-add-property n (make-sgf-property "B" "xy"))
;;   (sgf-node-value-list n "B")
;; ;  (sgf-node-to-string n)
;; ;  n
;; )





;; (let ((go (make-go-game))
;;     )
;;   (assert (go-game-p go))
;;   (assert (go-game-empty-p go))
;;   (go-game-sgf-add-property go "GE" "life and death")
;;   (sgf-node-p (go-game-current-node go))
;;   (go-game-prev-node go)
;;   (go-game-prev-node go)
;;   (go-game-sgf-search-property go "GE")
;;   (go-game-place-stone go 'black 1 2)
;;   (go-game-place-stone go 'white 19 3)
;;  (go-game-remove-stone go 1 2)
;;   (go-game-set-triangle go 1 1)
;;   (go-game-remove-property go 1 1)
;; ;  (my-tree-parent-node (go-game-tree go))

;;   (go-game-change-color go  'black)
;;   (go-game-move-stone go  4 4 t)
;;   (go-game-set-triangle go 4 4)

;;  (go-game-move-stone go 3 3 t)
;;  (go-game-capture-stone go 4 4)

;;  (go-game-move-stone go  2 2 t)
;; ; (go-game-stack go)
;; ; (go-game-prev-move go)
;; ; (go-game-prev-move go)
;; ; (go-game-next-move go)
;; ; (go-game-next-move go)

;;  (go-game-print-board go) 
;;  (insert 
;;   (go-game-print-sgf go) "\n"
;;   (format "%s" (go-game-agehama-black go)))
;;  (go-game-current-color go) 
;;  (assert (not (go-game-empty-p go)))
;;  (go-game-to-sgf-string (make-go-game-with-position go))

;; )






;; (let* ((tree  (car (make-sgf-tree-list-from-file 
;; 		    "~/src/GoGrinder/problems/goproblemsSGF/k02/6235.sgf")))
;;        (go (make-go-game tree)))
;;   (go-game-last-move go)
;;   (go-game-print-board go)
;;   )

