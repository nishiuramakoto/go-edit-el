
(defvar gx-directory "/home/makoto/go/prob")
(defvar gx-level)
(setq gx-excersize-time (* 60 5))
(setq gx-do-shuffle nil)

(setq gx-file-vector)
(setq gx-set-count 0)
(setq gx-file-counter 0)

(setq gx-buffer nil)
(setq gx-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "x" 'gx-start-excersize-one-time)
	(define-key map "s" 'gx-start-excersize-repeatedly)
	map))
(setq gx-message
"Go Excersize Mode (level:%x time limit:%s)
Press x or s to start a new excersize.
Press return to finish each problem.
")

;; for shuffle-vector
(require 'cookie1) 
(require 'cl)


(defun gx-make-file-list (level)
  (let ((full-name t)
	(match-regexp (format  "\\b%d-[0-9]+-.\\." level))
	(nosort nil)
	)
    (directory-files gx-directory full-name match-regexp nosort)))

(defun gx-make-file-vector (level shuffle)
  (let ((file-vec  (apply 'vector (gx-make-file-list level))))
    (if shuffle
	(shuffle-vector file-vec)
      file-vec)))


(defun gx-make-problem-vector (file-vec first-prob)
  (assert (>= first-prob 0))
  (assert (<  first-prob (length file-vec)))
  (let* ((len (length file-vec))
	 (first-part (subseq file-vec first-prob len))
	 (second-part (if (= first-prob 0) nil
			(subseq file-vec 0 first-prob))))
    (vconcat first-part second-part)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package dependent member functions


(defun gx-init (level shuffle)
  (setq gx-file-vector (gx-make-file-vector level shuffle))
  (setq gx-file-counter 0))
(defun gx-inc-counter ()
  (setq gx-file-counter (% (+ 1 gx-file-counter) (length gx-file-vector))))

(defun gx-make-excersize-command ()
  (let* ((command "feh --auto-zoom --full-screen  --hide-pointer -d -A \"emacsclient -n -e \\\" (gx-inc-counter) \\\"  \" ")
	 (space  " ")
	 (args (apply 'append (mapcar (lambda (x) (list x space)) (gx-make-problem-vector gx-file-vector gx-file-counter)))))
    (apply 'concat command space args)))


(defun gx-start-excersize-one-time ()
  (interactive)
  (setq gx-file-counter-before gx-file-counter)
  (setq gx-process (start-process  "gx" nil "sh" "-c" (gx-make-excersize-command)))
  (process-kill-without-query gx-process)
  (assert (processp gx-process))
  (setq gx-timer (run-at-time gx-excersize-time nil  'gx-finish-excersize gx-process)))

(defun gx-start-excersize-repeatedly ()
  (interactive)
  (setq gx-file-counter-before gx-file-counter)
  (setq gx-process (start-process  "gx" nil "sh" "-c" (gx-make-excersize-command)))
  (setq gx-timer (run-at-time t gx-excersize-time     'gx-update-excersize-status gx-process))
  (set-process-sentinel gx-process 'gx-excersize-sentinel))
  
(defun gx-excersize-sentinel (process event)
  (cancel-timer gx-timer))

(defun gx-update-excersize-status (process)
  (message "gx-update-excersize-status")
  (let ((finished (- gx-file-counter gx-file-counter-before)))
    (when  (< finished 0) 
      (incf finished (length gx-file-vector))
      (gx-init gx-level gx-do-shuffle))
    (when (> finished 0)
      (with-current-buffer  gx-buffer
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert (format "%s / %s : %s\n"  finished gx-excersize-time (current-time-string)))))))
  (setq gx-file-counter-before gx-file-counter))


(defun gx-finish-excersize (process)
  (assert (processp process))
  (gx-update-excersize-status process)
  (cancel-timer gx-timer)
  (delete-process process))




(defun gx-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gx-mode)
  (setq mode-name "go-excersize")
  (setq buffer-read-only t)
  (use-local-map gx-mode-map))

(defun gx-reshuffle ()
  (interactive)
  (setq gx-do-shuffle (y-or-n-p "random shuffle? "))
  (gx-init gx-level gx-do-shuffle))

(defun goexcersize (level)
  (interactive "nLevel? ")
  (setq gx-level level)
  (gx-init level gx-do-shuffle)
  (setq gx-buffer (switch-to-buffer  "*goexcersize*"))
  (insert (format gx-message  gx-level gx-excersize-time))
  (gx-mode))
  
