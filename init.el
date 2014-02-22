;; Load paths and files
(add-to-list 'load-path "~/.emacs.d/")

(mapc (lambda (x) (add-to-list 'load-path (concat "~/.emacs.d/" x "/"))) 
      '("popup" "deferred" "ctable" "epc" "jedi" 
	"autopair" "auto-complete" 
	"scala-mode2" "sbt-mode" 
	"projectile" "s" "dash" "pkg-info" "epl" 
	"helm" 
	"shell-pop"))

(mapc 'load
      '("custom_functions" 
	"powerline_tweak" 
	"tabbar_tweak"))

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

;; Python stuff
(require 'ctable)
(require 'epc)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; Stuff
(load-theme 'tango-dark) ; Sets colour theme
(tool-bar-mode -1) ; No toolbar
(delete-selection-mode 1) ; Delete selected text when typing
(setq-default cursor-type 'bar) ; Use bar for cursor
(show-paren-mode 1) ; Matches parentheses

(global-linum-mode t) ; Sets line numbers
(setq linum-format "%3d\u2502") ; Adds line next to line numbers

(global-hl-line-mode 1) ; Highlights current line
(set-face-background 'hl-line "#3e4446") ; Set colour of highlighted line
(set-face-foreground 'highlight nil) ; Keep syntax highlighting on current line

(setq scroll-error-top-bottom t) ; Allows pgup/pgdn to go to extremes
(setq next-screen-context-line 3) ; 3 line overlap on page scroll

(setq c-default-style "linux" c-basic-offset 4) ; Supposed to fix indentation

(global-set-key [home] 'smart-home) ; Smart home key
(global-set-key (kbd "S-<home>") 'smart-home-with-mark) ; Smart home key with selection

(global-set-key [f5] (lambda () (interactive)(kill-buffer))) ; kill current buffer with f5

(desktop-save-mode 1) ; save sessions

(setq dired-listing-switches "-aghopBG --group-directories-first") ; dired formatting
(put 'dired-find-alternate-file 'disabled nil) ; use same buffer on 'a'

(require 'autopair)
(autopair-global-mode) ; to enable in all buffers

(require 'shell-pop)
(global-set-key [f8] 'shell-pop) ; popup shell

(require 'tramp)
(setq tramp-default-method "ssh") ; use ssh for remote files

;; Auto-completion
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)
(setq ac-delay 0.05)
(setq ac-quick-help-delay 0.25)
(global-auto-complete-mode t)

;; Ensime + Scala + SBT
(add-to-list 'load-path "/usr/share/ensime/elisp")
(add-to-list 'exec-path "/usr/share/ensime")

(require 'scala-mode2)
(require 'sbt-mode)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-to-list 'auto-mode-alist '("\\.java\\'" . scala-mode))
(modify-coding-system-alist 'file "\\.java$" 'utf-8)

;; IDO settings
(require 'ido)
(ido-mode 0) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Async Shell Command"))
(ido-everywhere 1)
(setq ido-use-faces nil)

;(require 'projectile)
;(projectile-global-mode)
;(setq projectile-enable-caching t)

;; Helm
;; (require 'helm)
;; (require 'helm-files)
;; (require 'helm-projectile)
;; (setq helm-for-files-preferred-list
;;   '(helm-source-buffers-list
;;     helm-source-recentf
;;     ;helm-source-bookmarks
;;     helm-source-file-cache
;;     helm-source-files-in-current-dir
;;     helm-source-locate)
;;    ; helm-source-projectile-files-list)
;; )
;; (global-set-key (kbd "C-x C-f") 'helm-for-files)

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
