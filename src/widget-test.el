(require 'font-lock)
(require 'cl)
(require 'my-util)
(require 'widget)
     
(eval-when-compile
  (require 'wid-edit))
     
(defvar widget-test-repeat)
(defvar widget-test-font-lock-keywords)


(setq widget-test-font-lock-keywords
      `(
	(,(my-regexp-add-properties-func (my-quote-regex "(<em>)\\s*(.*?)\\s*(</em>)")
					 nil '(invisible t) nil  '(invisible t))
	 (2 'border t nil))
	))
	

(defun widget-notify-test (widget changed-widget &optional ev)
  (message (format "notified:%s ev=%s" (widget-value widget) ev)))
(defun widget-action-test (widget &optional ev)
  (message (format "action:%s ev=%s" (widget-value widget) ev)))

  


(defun widget-test ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Test*")
  (kill-all-local-variables)
  (make-local-variable 'widget-test-repeat)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (my-erase-widget-buffer)
    (erase-buffer)
    )
  (widget-insert "<em>  emphasized <gaiji=x> should be here. </em> \n")
  (widget-insert "Here is some documentation.\n\nName: ")
  (widget-create 'editable-field
     		 :size 13
		 :notify 'widget-notify-test
		 :action 'widget-action-test
		 )
  (widget-create 'menu-choice
     		 :tag "Choose"
     		 :value "This"
     		 :help-echo "Choose me, please!"
     		 :notify (lambda (widget &rest ignore)
     			   (message "%s is a good choice!"
     				    (widget-value widget)))
     		 '(item :tag "This option" :value "This")
     		 '(choice-item "That option")
     		 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-insert "Address: ")
  (widget-create 'editable-field
     		 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
     		 :notify (lambda (&rest ignore)
     			   (widget-value-set widget-test-repeat
     					     '("En" "To" "Tre"))
     			   (widget-setup))
     		 "other work")
  (widget-insert
   " for more information.\n\nNumbers: count to three below\n")
  (setq widget-test-repeat
     	(widget-create 'editable-list
     		       :entry-format "%i %d %v"
     		       :notify (lambda (widget &rest ignore)
     				 (let ((old (widget-get widget
     							':test-length))
     				       (new (length (widget-value widget))))
     				   (unless (eq old new)
     				     (widget-put widget ':test-length new)
     				     (message "You can count to %d." new))))
     		       :value '("One" "Eh, two?" "Five!")
     		       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
     		 :notify (lambda (&rest ignore) (message "Tickle"))
     		 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
     		 :value "One"
     		 :notify (lambda (widget &rest ignore)
     			   (message "You selected %s"
     				    (widget-value widget)))
     		 '(item "One") '(item "Another One.") '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
     			   (if (= (length (widget-value widget-test-repeat))
     				  3)
     			       (message "Congratulation!")
     			     (error "Three was the count!")))
     		 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
     		 :notify (lambda (&rest ignore)
     			   (widget-test))
     		 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords widget-test-font-lock-keywords)
  (font-lock-fontify-keywords-region (point-min) (point-max) nil)
  (widget-setup)
  (message "ok")
  (goto-char (point-min)))


