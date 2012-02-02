(require  'gnugo-xpms)
(require 'cl)

(defun dump-file (file data)
  "dump DATA to FILE"
  (with-temp-buffer 
    (insert (format "%s" data))
    (write-region (point-min) (point-max) file)))

(defun convert-xpm (infile outfile)
  (shell-command (format "/home/makoto/src/myelisp/src/convert-moku.sh %s %s" infile outfile)))

(defun file-contents (file)
  "return file contents as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun make-new-gnugo-xpms ()
  (let ((default-directory "/tmp/"))
    (loop for x in gnugo-xpms
	  for key = (first x)
	  for image = (cddr x)
	  for type =  (car key)
	  for place = (cdr key)
	  for data  = (second (memq :data image))
	  for name  = (format "%s-%d.xpm" type place)
	  for newname = (format "%s-%d-conv.xpm" type place)
	  for xpm = nil
	  do
	  (dump-file name data)
	  (convert-xpm name newname)
	  (setq xpm (list (cons type place) 'image :type 'xpm :data (file-contents newname)))
	  collect xpm
	  )))

(setq gnugo-xpms-small (make-new-gnugo-xpms))
(setq game-board-xpms gnugo-xpms-small)
(setq go-stone-image-cache nil)
(setq game-board-force-redraw nil)

