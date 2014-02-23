;; Load paths and files
(add-to-list 'load-path "~/.emacs.d/")

(mapc (lambda (x) (add-to-list 'load-path (concat "~/.emacs.d/" x "/"))) 
      '("popup" "deferred" "ctable" "epc" "jedi" 
	"autopair" "auto-complete" 
	"scala-mode2" "sbt-mode" 
	"sr-speedbar"
	;"weechat" "s"
	;"projectile" "s" "dash" "pkg-info" "epl" 
	;"helm" 
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

;; Stuff
(setq inhibit-startup-screen t)

(require 'sr-speedbar)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-delete-windows t)
(setq sr-speedbar-width 50)
(setq sr-speedbar-right-side nil)
(global-set-key [f7] 'sr-speedbar-toggle)

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

(setq dired-listing-switches "-aghoBG --time-style=+ --group-directories-first") ; dired formatting
(put 'dired-find-alternate-file 'disabled nil) ; use same buffer on 'a'
(add-hook 'dired-mode-hook 'no-linum)

(require 'autopair) ; pairs ", {, (, etc
(autopair-global-mode)

(require 'shell-pop) ; popup shell
(shell-pop--set-shell-type 'shell-pop-shell-type '("ansi-term" "*Terminal*" (lambda nil (ansi-term shell-pop-term-shell))))
;(setq shell-pop-shell-type (quote ("Custom" "*Terminal*" (lambda nil (ansi-term shell-pop-term-shell)))))
(setq shell-pop-window-height 60)
(setq shell-pop-window-position "bottom")
(add-hook 'term-mode-hook 'no-linum)
(add-hook 'shell-mode-hook 'no-linum)
(global-set-key [f8] 'shell-pop)

(require 'tramp)
(setq tramp-default-method "ssh") ; use ssh for remote files

(require 'ido)
(ido-mode 0) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Async Shell Command"))
(ido-everywhere 1)
(setq ido-use-faces nil)

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

;; Python stuff
(require 'ctable)
(require 'epc)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;(add-hook 'after-init-hook #'global-flycheck-mode)
