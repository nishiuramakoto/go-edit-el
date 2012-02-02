;; -*- mode:lisp-interaction ; -*-
(require 'mylib)
(makunbound 'go-edit-mode-keyboard-map)
(makunbound 'go-edit-mode-mouse-map-list)
(makunbound 'go-edit-board-coord)

(setq go-edit-overwrite-move nil)
(setq go-edit-display-image-flag t)
(setq go-edit-initial-position '(16 4))
(setq go-edit-auto-save-idle-time 5)
(setq go-edit-verbose-message nil)
(setq go-edit-error-threshold 5)
(setq go-game-default-white nil)


(load-default "gnugo-xpms")
(load-default "sgf")
(load-default "my-tree")
(load-default "goban")
(load-default "game-board")
(load-default "go-game")
(load-default "go-edit")

(add-hook 'go-edit-exit-hook 'my-go-edit-exit)
(remove-hook 'go-edit-exit-hook 'my-go-edit-exit)

;(elp-instrument-package "go-game-")
;(elp-instrument-package "my-")
;(elp-instrument-list '(overlay-put make-overlay delete-overlay overlay-get)) 
;(elp-set-master 'go-edit-new-board)
;(elp-reset-all)

(progn
  (let ((tree (make-my-tree)))
    (my-tree-set-node tree 'a)
    (my-tree-set-node tree 'b)
    (my-tree-add-child tree 'c)
    (my-tree-pop tree)
    (my-tree-add-child tree 'd)
    (my-tree-insert-parent tree 'e)
    tree))

(defun go-edit-use-small-xpm ()
  (interactive)
;  (load-default "gnugo-xpms-small")
  (setq game-board-xpms gnugo-xpms-small)
  (setq go-stone-image-cache nil))


;(with-current-buffer "*go-edit*"
;  (let ((go (goban-game goban)))
;    (go-game-current-color go)
;    (goban-player-color goban)
;    (go-game-check-next-move go 'black 10 10)
;    (goban-move goban 10 10)
;    ))







(defun my-go-edit-exit ()
;  (message "go-edit exit")
  (go-edit-write-sgf-file go-edit-sgf-default-filename t))


;(sgf-split-to-directory "~/sgf/9kyu-1kyu.sgf" "/tmp/sgf")
;; (make-my-tree-list-from-sgf-file "~/sgf/tmp.sgf")
;; (with-temp-buffer
;;   (insert-file-contents "~/sgf/tmp.sgf")
;;   (first (sgf-collection))
;;   )






