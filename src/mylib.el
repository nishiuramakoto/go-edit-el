

(defmacro defvar-local (symbol &optional init docstring)
  "Declare a buffer-local variable."
  `(progn 
     (defvar ,symbol ,init ,docstring)
     (make-variable-buffer-local (quote ,symbol))))

(defsubst my-random-elm (list)
  "Pick up an element from a list randomly."
  (let ((n (length list)))
    (nth (random n) list)))

(defun load-default (lispfile)
  (load (concat default-directory lispfile)))

(defun my-call-process (program &rest args)
  (let ((infile nil)
	(buffer "*my-call-process*")
	(display nil))
    (apply 'call-process program infile buffer display args)))

(defun my-call-process-async (program &rest args)
  (let ((name program)
	(buffer nil))
    (apply 'start-process name buffer program args)))

(defun my-join (w &rest args)
  (case (length args)
    (0  nil)
    (1  (format "%s" (first args)))
    (t  (format "%s%s%s" (first args) w (apply 'my-join w (rest args))))))




(provide 'mylib)
