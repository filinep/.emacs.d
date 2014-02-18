(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; Proxy
(setq url-http-proxy-basic-auth-storage
    (list (list (get-string-from-file "~/.emacs.d/proxy")
                (cons "Input your LDAP UID !"
                      (base64-encode-string (get-string-from-file "~/.emacs.d/pwd"))))))

;; Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")))
  )

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/popup/")
(add-to-list 'load-path "~/.emacs.d/deferred/")
(add-to-list 'load-path "~/.emacs.d/ctable/")
(add-to-list 'load-path "~/.emacs.d/epc/")
(add-to-list 'load-path "~/.emacs.d/jedi/")

(require 'ctable)
(require 'epc)
(autoload 'jedi:setup "jedi" nil t)

;; el-get
;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;(unless (require 'el-get nil 'noerror)
;  (with-current-buffer
;      (url-retrieve-synchronously
;       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;    (let (el-get-master-branch)
;      (goto-char (point-max))
;      (eval-print-last-sexp))))
;(el-get 'sync)

;; Smart home key
(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)

;; GUI
(load-theme 'tango-dark) ; Sets colour theme
(tool-bar-mode -1) ; No toolbar
(delete-selection-mode 1) ; Delete selected text when typing
(setq-default cursor-type 'bar) ; Use bar for cursor
(load "powerline_tweak.el") ; Sets modeline
(load "tabbar_tweak.el") ; Setup tabbar
(show-paren-mode 1) ; Matches parentheses

(add-to-list 'load-path "~/.emacs.d/autopair")
(require 'autopair)
(autopair-global-mode) ; to enable in all buffers

(global-linum-mode t) ; Sets line numbers
(setq linum-format "%3d\u2502") ; Adds line next to line numbers

(global-hl-line-mode 1) ; Highlights current line
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

(setq scroll-error-top-bottom t)
(setq next-screen-context-line 3)

;; SSH stuff
(require 'tramp)
(setq tramp-default-method "ssh")

;; Auto-completion
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)
(setq ac-delay 0.05)
(setq ac-quick-help-delay 0.5)
(global-auto-complete-mode t)

;; Ensime + Scala + SBT
(add-to-list 'load-path "~/.emacs.d/scala-mode2/")
(add-to-list 'load-path "/usr/share/ensime/elisp")
(add-to-list 'exec-path "/usr/share/ensime")
(add-to-list 'load-path "~/.emacs.d/sbt-mode/")

(require 'scala-mode2)
(require 'sbt-mode)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Python stuff
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; IDO settings
(require 'ido)
(ido-mode 1)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Async Shell Command"))

(add-to-list 'load-path "~/.emacs.d/helm/")
(require 'helm)
(require 'helm-files)
(setq helm-for-files-preferred-list
  '(helm-source-buffers-list
    helm-source-recentf
    helm-source-bookmarks
    helm-source-file-cache
    helm-source-files-in-current-dir
    helm-source-locate)
)
(global-set-key (kbd "C-x C-f") 'helm-for-files)

;; Dired sorting order
(setenv "LC_COLLATE" "C")

;; Popup terminal
(add-to-list 'load-path "~/.emacs.d/shell-pop")
(require 'shell-pop)
(global-set-key [f8] 'shell-pop)

;; Custom stuff I didnt change
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" default)))
 '(inhibit-startup-screen t)
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-height 60)
 '(shell-pop-window-position "bottom")
)
(put 'dired-find-alternate-file 'disabled nil)
