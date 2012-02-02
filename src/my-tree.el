
;; See elisp manual
(require 'cl)
;(eval-when-compile (require 'cl))

;; my-tree is a tuple of a traversing stack , a binary tree in natural list representation , 
;; and a predicate to test whether a node is a leaf or not (default:x is leaf iff (atom x)) 



(defstruct  (my-tree 
	     (:constructor nil)
	     (:constructor make-my-tree (&optional 
					 (leaf-test 'atom)
					 (printer 'prin1-to-string)
					 (tree-data nil)
					 &aux (tree (cons 'my-tree-root tree-data))
					      (stack (list tree)) )))
  stack tree  leaf-test printer)



;; Frequently used private utility methods
(defsubst my-tree-push (x tree)
  (push x  (my-tree-stack tree)))
(defsubst my-tree-pop (tree)
  (pop (my-tree-stack tree)))
(defsubst my-tree-top (tree)
  (car (my-tree-stack tree)))
(defsubst my-tree-get-element (node)
  (car node))

;; Basic predicates
;; For all the tree nodes , either my-tree-branch-p or my-tree-node-list-p must hold.
(defsubst my-tree-empty-p (tree)
  (assert (consp  (my-tree-tree tree)))
  (not (cdr (my-tree-tree tree))))

(defsubst my-tree-branch-p (p leaf-test)
  "t iff p is at ((a)(b)...)"
  (and (consp p) (consp (car p)) (funcall leaf-test (caar p))))
(defsubst my-tree-node-list-p (p leaf-test)
  "t iff p is at (a b c..) where a is either a user defined leaf,or the root node"
  (and (consp p) (or (eq (car p) 'my-tree-root) (funcall leaf-test (car p)))))
(defsubst my-tree-node-p (tree node)
  (my-tree-node-list-p node (my-tree-leaf-test tree)))


(defsubst my-tree-at-branch-p (tree &optional node)
  (setq node (or node (my-tree-top tree)))
  (assert (my-tree-node-p tree node))
  (my-tree-branch-p node (my-tree-leaf-test tree)))

(defsubst my-tree-at-last-node-p (tree &optional node)
  "true iff the node has no children"
  (setq node (or node (my-tree-top tree)))
  (assert (my-tree-node-p tree node))
  (null (cdr node)))


(defsubst my-tree-before-branch-p (tree &optional node)
  "true iff next node is a branch node."
  (setq node (or node (my-tree-top tree)))
  (assert (my-tree-node-p tree node))
  (my-tree-branch-p (cdr node) (my-tree-leaf-test tree)))

(defsubst my-tree-at-first-node-p (tree)
  (assert (consp (my-tree-stack tree)))
  (null (cdr (my-tree-stack tree))))
;; End predicates

(defun my-tree-canonicalize (p leaf-test)
  "(ab) -> (a(b))  but (a(b)) -> (a(b))"
  (assert (my-tree-node-list-p p leaf-test) t "1:not node list %s %s")
  (let ((q (cdr p)))
    (when (my-tree-node-list-p q leaf-test)
      (setcdr p (list q)))
    (assert (or (null (cdr p)) (my-tree-branch-p (cdr p) leaf-test)) t "2:not node list %s %s" )
    )
  p)

(defun my-tree-canonicalize-top (tree)
  (let ((p (my-tree-top tree))
	(test (my-tree-leaf-test tree)))
    (my-tree-canonicalize p test)))

(defun my-tree-simplify (p leaf-test)
  "(a(b)) -> (ab)  but  (a(b)(c)) ->(a(b)(c)) "
  (assert (my-tree-node-list-p p leaf-test) t "3 %s %s" )
  (let ((q (cdr p)))
    (when (and (my-tree-branch-p q leaf-test) (null (cdr q)))
      (setcdr p (car q))))
  p)

(defun my-tree-simplify-top (tree)
  (let ((p (my-tree-top tree))
	(test (my-tree-leaf-test tree)))
    (my-tree-simplify p test))
  tree)


(defun my-tree-setpos (tree)
  "roll back to the last non-branch node"
  (let ((leaf-test (my-tree-leaf-test tree)))
    (assert (my-tree-stack tree))
    (while (my-tree-branch-p (my-tree-top tree) leaf-test)
      (my-tree-pop tree)
      (assert (my-tree-stack tree))))
  tree)

  

;; public methods


(defun my-tree-child-node-list (tree &optional node)
  "Return the list of all children nodes that belong to NODE (or the current node if nil)"
  (setq node (or node (my-tree-top tree)))
  (assert (my-tree-node-p tree node))
  (if (my-tree-before-branch-p tree node)
      (cdr node)
    (let ((only-child (cdr node)))
      (and only-child (list only-child)))))

(defun my-tree-set-node (tree element)
  "Remove the subtree after current position,and append a node containing ELEMENT,then advance to the node."
  (let* ((leaf-test (my-tree-leaf-test tree))
	 (p (my-tree-top tree)))
    (assert (consp p))
    (assert (funcall leaf-test element))
    (setcdr p (list element))
    (my-tree-push (cdr p) tree))
  tree)
(defun my-tree-insert-node (tree element)
  "Insert a node containing ELEMENT  after the current node,then advance to the inserted node."
  (let* ((leaf-test (my-tree-leaf-test tree))
	 (p (my-tree-top tree))
	 (q (cdr p)))
    (assert (consp p))
    (assert (funcall leaf-test element))
    (setcdr p (cons element q))
    (my-tree-push (cdr p) tree))
  tree)
  
(defun my-tree-insert-parent (tree node)
  "Make NODE to be the only child of current node's parent,and the parent of current node's sibling(s) (including the current node.) The tree stack is adjusted to re-position at the current node."
  (let ((top (my-tree-top tree)))
    (my-tree-pop    tree)
    (my-tree-insert-node tree node)
    (my-tree-next-node tree top)))

(defun my-tree-current-node (tree)
  "return current node"
  (assert (not (my-tree-at-branch-p tree)))
  (first (my-tree-top tree)))

(defun my-tree-next-node (tree &optional child)
  "move to the first of current node's children
1: (ab)      
2: (a(b)(c))
3: (a)  -> error
"
  (let ((children (my-tree-child-node-list tree)))
    (unless child
      (setq child (first children)))
    (if (not (memq child children))
	(error "No such child %s" child))
    (my-tree-push child tree)))

(defun my-tree-search-child-node (tree child-test)
  "Return a list of child nodes whose element matches CHILD-TEST."
  (remove-if-not (lambda (child) (funcall child-test (car child))) (my-tree-child-node-list tree)))


(defun my-tree-child-element-list (tree &optional node)
  (mapcar 'car (my-tree-child-node-list tree node)))

(defun my-tree-sibling-node-list (tree)
  (let* ((top (my-tree-top tree))
	 (current-node (first top)))
    (assert (not (my-tree-at-first-node-p tree)))
    (my-tree-prev-node tree)
    (prog1 
	(my-tree-child-node-list tree)
      (my-tree-push top tree ))))

(defun my-tree-sibling-element-list (tree)
  (let* ((top (my-tree-top tree))
	 (current-node (first top)))
    (assert (not (my-tree-at-first-node-p tree)))
    (my-tree-prev-node tree)
    (prog1 
	(my-tree-child-element-list tree)
      (my-tree-push top tree ))))

(defun my-tree-get-next-sibling (tree)
  (let* ((node (my-tree-current-node tree))
	 (sibling-list (my-tree-sibling-element-list tree))
	 (tail         (memq node sibling-list))
	 (next-node    (second  tail)))
    (assert sibling-list)
    (or next-node (car sibling-list))))

(defun my-tree-get-next-sibling-node (tree)
  (let* ((node (my-tree-top tree))
	 (sibling-list (my-tree-sibling-node-list tree))
	 (tail         (memq node sibling-list))
	 (next-node    (second  tail)))
    (assert sibling-list)
    (or next-node (car sibling-list))))

(defun my-tree-get-prev-sibling (tree)
  (let* ((node (my-tree-current-node tree))
	 (sibling-list (reverse (my-tree-sibling-element-list tree)))
	 (tail         (memq node sibling-list))
	 (next-node    (second  tail)))
    (assert sibling-list)
    (or next-node (car sibling-list))))

(defun my-tree-get-prev-sibling-node (tree)
  (let* ((node (my-tree-top tree))
	 (sibling-list (reverse (my-tree-sibling-node-list tree)))
	 (tail         (memq node sibling-list))
	 (next-node    (second  tail)))
    (assert sibling-list)
    (or next-node (car sibling-list))))


(defun my-tree-prev-node (tree)
    (my-tree-pop tree)
    (my-tree-setpos tree))

(defun my-tree-parent-node (tree)
  (if (my-tree-at-first-node-p tree)
      nil
    (let ((current-node (my-tree-top tree))
	  parent-node)
      (my-tree-prev-node tree)
      (setq parent-node (my-tree-current-node tree))
      (my-tree-next-node tree current-node)
      parent-node)))

(defun my-tree-move-right (tree)
  "move to the right node unless it is nil"
  (my-tree-push (cdr (my-tree-top tree)) tree))

(defun my-tree-move-down (tree)
  "move down if current node is a branch node"
  (assert (my-tree-at-branch-p tree))
  (my-tree-push (car (my-tree-top tree)) tree))


(defun my-tree-add-child (tree node)
  "add new child to current node and move to that child.
e.g. (ab) -> (a(b)(c)) and (a) -> (ac)"
  (let ((new-child (list node)))
    (my-tree-canonicalize-top tree)
    (nconc (my-tree-top tree)  (list new-child))
    (my-tree-simplify-top tree)
    (assert (memq new-child (my-tree-child-node-list tree)))
    (my-tree-push new-child tree)))
 
(defun my-tree-del-child (tree &optional child)
  "Delete the specified CHILD,or the first child if CHILD is nil"
  (my-tree-canonicalize-top tree)
  (unless child
    (setq child (first (my-tree-child-node-list tree))))
  (delq child (my-tree-top tree))
  (my-tree-simplify-top tree))

(defun my-tree-delete-all-child (tree)
  (while (my-tree-child-node-list tree)
    (my-tree-del-child tree)))

(defun my-tree-del-node (tree)
  "Delete the current node and its subtree,and pop back to the parent"
  (let ((this-tree (my-tree-top tree)))
    (my-tree-pop tree)
    (my-tree-del-child tree this-tree)))

(defun my-tree-go-to-root (tree)
  (while (not (my-tree-at-first-node-p tree))
    (my-tree-prev-node tree))
  (assert  (my-tree-at-first-node-p tree)))

(defun my-tree-root-node (tree)
  (assert (my-tree-node-p tree (my-tree-tree tree)))
  (my-tree-tree tree))


;; TODO: make it non-recursive for speed
(defun my-tree-print-to-string (tree)
  (let ((tree-rep (cdr (my-tree-tree tree)))) ;; strip leading my-tree-root symbol
    (my-tree-print-to-string1 tree-rep  (my-tree-leaf-test tree) (my-tree-printer tree))))

(defun my-tree-print-to-string1 (tree-rep leaf-test printer )
  (mapconcat (lambda (x) (my-tree-print-each x leaf-test printer )) tree-rep nil) )

(defun my-tree-print-each (x leaf-test printer)
  (if (funcall leaf-test x)
      (funcall printer x)
    ;; x is list
    (concat "(" (my-tree-print-to-string1 x leaf-test printer) ")")))

(defun my-tree-pop-to-node (tree node)
  (while (not (eq node (my-tree-top tree)))
    (assert (not (my-tree-at-first-node-p tree)))
    (my-tree-prev-node tree)))

		 
(provide 'my-tree)
