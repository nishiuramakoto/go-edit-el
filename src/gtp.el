;; Library for interaction with gnugo via Go Text Protocol

;; I would like to require that all the private functions start with gtp-- prefix, whereas
;; public functions start with gtp- prefix,but this is not easily done because of
;; the way defstruct generates its members.
;; I nevertheless use gtp-- (or gtp-session--) prefix for non-idempotent internal-only functions 
;;  which might break consistency of the object. In other words,only functions deemed
;; not to introduce inconsistency *directly* are to start with gtp- prefix (even though
;; these  might include functions that provide access to private data structures,leading to
;; potential inconsistency of the object.)


(require 'cl)
(require 'mylib)


(defvar gtp-program-name "gnugo")
(defvar gtp-program-args '("--mode" "gtp"))
(defvar gtp-session-timeout 2.0)
(defvar gtp-process-buffer-name "*gtp process*")

(defun gtp-debug-message (str &rest args)
  (apply 'message (concat "gtp debug:" str) args))

(defstruct (gtp-session
	    (:constructor nil)
	    (:constructor make-gtp-session-internal (&optional (board-size 19) 
							       (black-stone-list nil)
							       (white-stone-list nil)
							       (komi 6.5)
							       &aux (komi (or komi 6.5))
							       &aux (board-size (or board-size 19))
							       &aux (process (gtp-make-process))
							       )))
  board-size
  black-stone-list
  white-stone-list
  komi
  process
  status
  error-message
  )

;; Make the object singleton at the moment
(defun gtp-make-process ()
  (if (get-buffer gtp-process-buffer-name)
      (error "buffer already exists:%s" gtp-process-buffer-name)
    (apply 'start-process "gtp process"  gtp-process-buffer-name gtp-program-name gtp-program-args)))

(defun make-gtp-session (&optional board-size black-stone-list white-stone-list komi)
  (let ((session (make-gtp-session-internal board-size black-stone-list white-stone-list komi)))
    (assert (gtp-session-is-valid session))
    (gtp-session--send-command session 'boardsize (gtp-session-board-size session))
    (assert (gtp-session-status session))
    (gtp-session--send-command session 'clear_board)
    (assert (gtp-session-status session))
    (gtp-session--send-command session 'komi (gtp-session-komi session))
    (assert (gtp-session-status session))
    (loop for stone in (gtp-session-black-stone-list session)
	  do 
	  (assert (gtp-valid-vertex stone))
	  (gtp-session--send-command session 'play 'black stone)
	  (assert (gtp-session-status session)))
    (loop for stone in (gtp-session-white-stone-list session)
	  do 
	  (assert (gtp-valid-vertex stone))
	  (gtp-session--send-command session 'play 'white  stone)
	  (assert (gtp-session-status session)))
    session))


(defun gtp-session-is-valid (session)
  (and (gtp-session-p session)
       (gtp-session-process session)
       (eq (process-status (gtp-session-process session)) 'run)
       ))

(defun gtp-valid-vertex (vertex)
  (and (sequencep vertex)
       (= 2 (length vertex))))
(defun gtp-format-vertex (vertex)
  (if (stringp vertex)
      vertex
    (assert (= 2 (length vertex)))
    (let ((x (elt vertex 0))
	  (y (elt vertex 1)))
      (assert (and (numberp x) (<= 0 x) (< x 19)) t "%s x=%s not in range" x)
      (assert (and (numberp y) (<= 0 y) (< y 19)) t "%s y=%s not in range" y)
      (format "%c%d" (elt "ABCDEFGHJKLMNOPQRSTUVWXYZ" x) (+ 1 y)))))

(defun gtp-parse-vertex (v)
  (let (x y)
;    (if (not (string-match "^\s_*\\(\\[A-Za-z\\]\\)\\(\\[0-9\]\\)\+" v))
    (if (not (string-match "\\([A-HK-Za-hk-z]\\)\\([0-9]+\\)" v))
	(error v))
    (setq x (- (string-to-char (upcase (match-string 1 v))) ?A))
    (when (>= x 9)
      (decf x))
    (setq y (string-to-number (match-string 2 v)))
    (decf y)
    (assert (and (<= 0 x) (< x 19)) t "x=%s not in range" x)
    (assert (and (<= 0 y) (< y 19)) t "y=%s not in range" y)
    (vector  x y)))


(defun gtp-session-buffer (session)
  (process-buffer (gtp-session-process  session)))

(defun gtp-compose-command (command &rest args)
  (apply 'my-join " " command args))




(defun gtp-protocol-output-finished (beg)
  (gtp-debug-message "searching string ... %s at %s" (buffer-substring beg (point-max)) beg)
  (save-excursion
    (goto-char beg)
    (search-forward "\n\n" nil t)))

(defun gtp-session-quit (session)
  (assert (gtp-session-is-valid session))
  (delete-process (gtp-session-process session))
  (setf (gtp-session-process session) nil))



(defun gtp-session--send-command (session command &rest args)
  (assert (gtp-session-is-valid session))
  (let* ((process (gtp-session-process session))
	 (buffer  (gtp-session-buffer session))
	 (current-mark-position (marker-position (process-mark process)))
	 (command-string (apply 'gtp-compose-command command args))
	 output)
    (assert process)
    (assert buffer)
    (gtp-debug-message "send command:%s %s" current-mark-position command-string)

    (with-current-buffer buffer
      (process-send-string process (concat command-string "\n"))
      (insert command-string "\n")
      (set-marker (process-mark process) (point-max))
      (setq current-mark-position (marker-position (process-mark process)))
      (while (not (gtp-protocol-output-finished current-mark-position))
	(when (not (accept-process-output process gtp-session-timeout))
	  (error (format "gtp session timeout (command=%s)" command-string))))
      (setq output (buffer-substring current-mark-position (process-mark process)))
      (assert output)
      (case (elt output 0)
	(?= (setf (gtp-session-status session) t)
	    output)
	(?? (setf (gtp-session-status session) nil)
	    (setf (gtp-session-error-message session) output)
	    (gtp-debug-message "gtp error:%s" output)
	    nil)
	(t  (error "GTP:Unknown output:%s" output))))))




(defun gtp-session-play (session color  x y)
  (let ((stone-list (gtp-session-stone-list session color)))
    (gtp-session--send-command session  'play color (gtp-format-vertex (vector x y)))
    (if (gtp-session-status session)
	(gtp-session-update session)
      (error "GTP error:%s" (gtp-session-error-message session)))))

(defun gtp-session-update (session)
  (list (gtp-session--update-stone-list session 'white)
	(gtp-session--update-stone-list session 'black)))

(defun gtp-session--update-stone-list (session color)
  (let* ((stone-list (gtp-session-stone-list session color))
	 (output (gtp-session--send-command session  'list_stones color))
	 (vertex-list (split-string output)))
    (assert (gtp-session-status session))
    (assert (eq ?= (elt (first vertex-list) 0)))
    (pop vertex-list)
    (setf stone-list (mapcar 'gtp-parse-vertex vertex-list))
    stone-list))


(defun gtp-session-stone-list (session color)
  (case color
    ('white (gtp-session-white-stone-list session))
    ('black (gtp-session-black-stone-list session))
    (t  (error "Invalid color:%s" color))))


(defun gtp-session-showboard (session)
  (gtp-session--send-command session 'showboard))


(defun gtp-session-captures (session color)
  (let ((output (gtp-session--send-command session 'captures color)))
    (assert (gtp-session-is-valid session))
    (string-to-number (second (split-string output)))))

(defun gtp-session-undo (session)
  (let ((output (gtp-session--send-command session 'undo)))
    (gtp-session-update session)))


(provide 'gtp)
