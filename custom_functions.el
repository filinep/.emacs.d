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

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
	    ))

