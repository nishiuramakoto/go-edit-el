;; See elisp manual
(require 'cl)
;(eval-when-compile (require 'cl))

;;; 

;;; regexp quoting
(defvar my-re-charmap-alist
  '(( "("  . "\\(") (")" . "\\)")  ( "\\\\s"  . "\\s-" ) ("|" "\\|")))

(defun my-string-match-at (re str &optional pos)
  "One might want to avoid using this function within a heavy loop.
   If RE matches STR at (or POS 0),return the length of the matched string,otherwise nil."
  (when (and pos (> pos 0))
    (setq str (substring str pos)))
  (when (string-match (concat "\\`" re)  str)
    (match-end 0)))

(defun my-quote-regex (str &optional re-char-map-alist)
  "Make sure to eval-when-compile when using this function."
  (loop with strlen = (length str)
	with pos = 0
	with alist = (or re-char-map-alist my-re-charmap-alist)
	for  ch-len = (or (some (lambda (x) (let ((len (my-string-match-at (car x) str pos)))
					      (when len (cons (cdr x) len))))
				alist )
			  (assert (< pos strlen))
			  (cons (substring str pos (+ 1 pos)) 1))
	do (incf pos (cdr ch-len))
	concat (car ch-len)
	while (< pos strlen)))


(defun my-match-string-list-no-properties ()
  (loop for x on (match-data) by 'cddr
	collect (buffer-substring-no-properties (car x) (cadr x))))
(defun my-match-string-list ()
  (loop for x on (match-data) by 'cddr
	collect (buffer-substring (car x) (cadr x))))

;; font-lock related
(defmacro my-regexp-add-properties-func (regex &rest props-list)
  "Returns a function to be used as a MATCHER in font-lock-keywords.
   N-th element of props-list is a property list that is to be attached
   to the string corresponding to the n-th subexpression of REGEX (0 being the whole match).
   If the element is a function,then it is called with no argument, and its return value is used
   as the property list to be attached. In the function,match data can be accessed as usual.
"
  `(lambda (end) (my-regexp-add-properties ,regex end ,@props-list )))
     

(defun my-regexp-add-properties (regex limit &rest props-list)
  (when (re-search-forward regex limit t)
    (loop for prop in props-list
	     for i from 0
	     do  (add-text-properties (match-beginning i) (match-end i) 
				      (if (functionp prop)  
					  (save-match-data (funcall prop))
					prop)))
    t))



; used like this:
(defvar my-font-lock-keywords
      `(
	(,(my-regexp-add-properties-func (my-quote-regex "(<em>)\\s*(.*?)\\s*(</em>)")
					 ;; properties other than face
					 nil '(invisible t) nil  '(invisible t))
	 (2 'border t nil))
	(,(my-regexp-add-properties-func "<gaiji=\\(.*?\\)>") . 'italic)
	))
	



;;;
(defun my-match-prefix (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (eq t (compare-strings s1 0 l1 s2 0 l1))))

(defun my-between-inclusive (x interval)
  (and (>= x (car interval)) (<= x (cdr interval))))
(defun my-contained-in-inclusive-intervals (x intervals)
  (some (lambda (interval) (my-between-inclusive x interval)) intervals))

(defun my-quote-a-char (char)
  (if (= char ?\\) "\\\\\\"
    (if (my-contained-in-inclusive-intervals char '((?a . ?z) (?A . ?Z) (?0 . ?9) ?. ?/ ?- ?_  (128 . 255)) )
	(string char)
      (string ?\\  char))))

;; helper functions

(defun my-make-repeated-string (string-to-repeat times &optional prefix postfix)
  (concat prefix (loop repeat times concat string-to-repeat) postfix))
(defun my-make-1parametrized-string (format-string start end &optional prefix postfix)
  (concat prefix
	  (loop for i from  start to (- end 1)
		concat (format format-string i))
	  postfix))

;A list of regexps which matches the old style function declarations as frequently appeas in GNU c code.a function declaration must start at the first column of a line.
(defun my-query-replace-regexp-alist (regexp-alist)
  "take list of the form (regexp-replace . string) ,and do query-replace-regexp for each pair "
  (dolist (regexp-pair regexp-alist)
    (query-replace-regexp (first regexp-pair) (rest regexp-pair) nil (point-min) (point-max) )))


(defun my-make-fprintf-stderr-regexp (num)
  (cons 
   (my-make-repeated-string " *, *\\([^\\,]*\\)" num "fprintf *(stderr *, *\\(\".*\"\\)" " *);") 
   (my-make-1parametrized-string " %% \\%d" 2 (+ num 2)  "cerr << boost::format(\\1)" ";") ))

(defun my-make-interval (start end)
  (loop for i from start to end collect i))
(defun my-make-fprintf-stderr-regexp-alist (start end)
  (mapcar 'my-make-fprintf-stderr-regexp  (my-make-interval start end)))

(defun my-do-query-replace-fprintf-stderr (num-of-args)
  " from:  fprintf ( stderr ,\"%d%d\" , x,y);  
    to  :  cerr << boost::format (\"%d%d\") % x % y; "
  (interactive "p")
  (my-query-replace-regexp-alist (my-make-fprintf-stderr-regexp-alist 1 (+ num-of-args 1) ) ))

;; Custom function definitions
(defun my-subtract-time (time1 time2)
  "returns float"
  (let* ((high1 (nth 0 time1))
	 (low1  (nth 1 time1))
	 (microsec1 (nth 2 time1))
	 (high2 (nth 0 time2))
	 (low2  (nth 1 time2))
	 (microsec2 (nth 2 time2))
	 (a 65536)
	 (b 1)
	 (c 1e-06)
	 (x1 (- high1 high2))
	 (x2 (- low1 low2))
	 (x3 (- microsec1 microsec2)))
    (+ (* a x1) (* b x2) (* c x3))))



(defmacro my-time (&rest f)
  `(let* ((my-time-t0 (current-time))
	  (result (progn ,@f))
	  (my-time-t1 (current-time))
	  (diff (my-subtract-time my-time-t1 my-time-t0)))
     (message (format "%f" diff))))




(defmacro my-save-input-method (&rest body)
  "Temporarily disables the current active input method (if any),and returns the result of executing BODY."
  (let ((active-input-method (gensym))
	(result (gensym)))
    `(let ((,active-input-method  current-input-method)
	   ,result )
       (if ,active-input-method
	   (set-input-method nil))
       (setq ,result (progn ,@body))
       (if ,active-input-method
	   (set-input-method ,active-input-method))
       ,result)))


;; Frame
;; asymptotically faster than the method used in select-frame-by-name
(defun my-find-frame-by-name (name)
  (some  (lambda (frame) 
	   (let ((fn (frame-parameter frame 'name)))
	     (and (equal name fn) frame)))
	 (frame-list)))

(defun my-find-frame-by-regexp (regex)
  (some  (lambda (frame) 
	   (let ((fn (frame-parameter frame 'name)))
	     (and (stringp fn) (string-match regex fn) frame)))
	 (frame-list)))




(defun my-remove-frame-buffer-list (frame buffer)
  (let* ((alist (frame-parameters frame))
	 (buffer-list (assoc 'buffer-list alist)))
    (delete buffer buffer-list)
    (assq-delete-all 'buffer-list alist)
    (push (cons 'buffer-list buffer-list) alist)
    (modify-frame-parameters frame alist)))


	 
;; Advice

;; (defmacro my-trace (func-name)
;;   (let ((func (intern func-name))
;; 	(advice-name (intern (concat "trace-" func-name))))
;;     (list 'progn
;; 	  (list 'defadvice func (list 'before advice-name (list '&rest 'args))
;; 		(list 'message "TRACE:%s %s" func-name 'args))
;; 	  (list 'ad-activate (list 'quote func)))))

;; (defmacro my-untrace (func-name &optional all)
;;   (let ((func (intern func-name))
;; 	(regexp (concat "^trace-" func-name "$")))
;;     (if all
;; 	(list 'ad-deactivate  (list 'quote func))
;;       (list 'ad-deactivate-regexp regexp))))

;; (defmacro my-toggle-trace (func-name)
;;   (let ((func (intern func-name)))
;;     (if (ad-is-active func)
;; 	(list 'my-untrace func-name)
;;       (list 'my-trace func-name))))
  

;; (defun my-current-func (&optional bound)
;;   (save-excursion
;;     (when (re-search-backward "^\\s *(\\s *\\(defun\\|defmacro\\)\\s *\\([^[:space:]()]+\\)" bound nil)
;;       (match-string 2))))

;; (defun my-toggle-trace-current-func (&optional bound)
;;   (interactive)
;;   (let ((func (my-current-func bound)))
;;     (my-toggle-trace func)))


(provide 'my-util)
