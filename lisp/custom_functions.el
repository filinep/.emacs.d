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

(defun neotree-project-root-dir-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the
current buffer directory."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        )
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        ;;(if file-name
        ;;   (neotree-find file-name))
        ;;(other-window)
        ))))

