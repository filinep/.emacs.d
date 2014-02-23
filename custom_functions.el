(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun is-buffer-visible (bName)
  "Return whether a buffer is visible."
  ;(let (x nil)
    (dolist (x (window-list)) (message "%s" x)))
;)

(defun smart-home ()
  "Smart home functions which toggles between the beginning
of the line and the first indented character"
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun smart-home-with-mark ()
  "Smart home with selection support"
  (interactive)
  (unless mark-active
    (push-mark)
    (call-interactively 'set-mark-command))
  (smart-home))

(defun dired-mouse-find-alternate-file (event)
  "In dired, visit the file or directory you click on instead of the dired buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-alternate-file (file-name-sans-versions file t))))

;(define-key dired-mode-map [mouse-2] 'dired-mouse-find-alternate-file)

(defun no-linum () (linum-mode 0))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
	    ))

