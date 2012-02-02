
(require 'go-game)
(require 'cl)
(require 'gtp)

(defstruct (go-game-with-gtp (:include go-game))
  (session (make-gtp-session)))


(defun go-game-make-default-sgf-tree ()
  (let ((new-tree (make-sgf-tree))
	(initial-node (make-sgf-node)))
    (my-tree-add-child new-tree initial-node)
    new-tree))

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



(defun go-game-tree-next-node (go &optional node)
  (my-tree-next-node (go-game-tree go) node))

(defun go-game-tree-prev-node (go)
  (my-tree-prev-node (go-game-tree go))
  (when (my-tree-at-first-node-p (go-game-tree go))
    (my-tree-next-node (go-game-tree go))))

(defun go-game-tree-delete-node (go)
  (my-tree-del-node (go-game-tree go)))


(defun go-game-delete-move (go)
  (let* ((tree (go-game-tree go))
	 (node (my-tree-top tree)))
    (go-game-prev-move go)
    (my-tree-del-child tree node)))
