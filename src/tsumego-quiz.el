

(require 'cl)
(require 'go-edit)


(makunbound 'tsumego-quiz-mode-map)
(defvar tsumego-quiz-mode-map
  (let ((map  (make-sparse-keymap)))
    (define-key map "\C-m"   'tsumego-quiz-exit)
    (define-key map [down-mouse-1] (tsumego-mouse-generic-command 'tsumego-quiz-guess-move))
    map)
  "keymap for tsumego quiz.")



(defun tsumego-quiz-wrong ()
  (setq tsumego-quiz-state 'wrong)
  (exit-recursive-edit))

(defun tsumego-quiz-correct ()
  (setq tsumego-quiz-state 'correct)
  (exit-recursive-edit))

(defun tsumego-quiz-guess-move ()
  (interactive)
  (let* ((move   (go-edit-choose-move-at-point)))
    (if (not move )
	(tsumego-quiz-wrong)
      (let* ((next-move-list (go-game-next-move-list tsumego-quiz-current-game))
	     (next-move      (my-random-elm next-move-list)))
	(when next-move-list
	  (tsumego-quiz-choose-move  (go-stone-column next-move) (go-stone-row next-move)))
	(when (tsumego-quiz-last-move-p)
	  (tsumego-quiz-correct))))))

(defun tsumego-quiz-exit ()
  (interactive)
  (setq tsumego-quiz-state 'exit)
  (exit-recursive-edit))


(defun tsumego-quiz (go-game)
  (setq tsumego-quiz-current-game (go-game-deep-copy go-game))
  (setq tsumego-quiz-state nil)
  (go-game-first-move tsumego-quiz-current-game)
;  (tsumego-quiz-set-color)
  (tsumego-quiz-draw)
  (goto-char (game-board-position tsumego-quiz-board 4 16))
  (recursive-edit)
  tsumego-quiz-state
  )

(provide 'tsumego-quiz)
