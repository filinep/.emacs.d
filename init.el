;; Load paths and files
(add-to-list 'load-path "~/.emacs.d/lisp")

(mapc (lambda (x) (add-to-list 'load-path (concat "~/.emacs.d/" x "/"))) 
      '("popup" "deferred" "ctable" "epc" "jedi" 
        "autopair" "auto-complete" 
        "scala-mode2" "sbt-mode" 
        "s"
        "git-gutter"
        "epl" "pkg-info.el" "f.el" "dash.el"
        "company-mode"
        "jade-mode" "coffee-mode"))

(require 'f)

(mapc 'load
      '("custom_functions" 
        "powerline_tweak" 
        "tabbar_tweak"
        ))

;; Proxy
(setq url-http-proxy-basic-auth-storage
    (list (list (get-string-from-file "~/.emacs.d/proxy")
                (cons "Input your LDAP UID !"
                      (base64-encode-string (get-string-from-file "~/.emacs.d/pwd"))))))

;; Stuff
(setq x-select-enable-clipboard t)
(unless window-system
 (when (getenv "DISPLAY")
  (defun xsel-cut-function (text &optional push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  (defun xsel-paste-function()
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
 ))

(setq inhibit-startup-screen t)

(load-theme 'tango-dark) ; Sets colour theme
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; No scrollbar
(menu-bar-mode -99) ; No menubar
(delete-selection-mode 1) ; Delete selected text when typing
(setq-default cursor-type 'bar) ; Use bar for cursor
(show-paren-mode 1) ; Matches parentheses

(global-subword-mode)

(global-hl-line-mode 1) ; Highlights current line
(set-face-background 'hl-line "#3e4446") ; Set colour of highlighted line
(set-face-foreground 'highlight nil) ; Keep syntax highlighting on current line

(setq scroll-error-top-bottom t) ; Allows pgup/pgdn to go to extremes
(setq next-screen-context-line 3) ; 3 line overlap on page scroll

(setq c-default-style "linux" c-basic-offset 4) ; Supposed to fix indentation

(global-set-key [home] 'smart-home) ; Smart home key
(global-set-key (kbd "C-a") 'smart-home)
(global-set-key (kbd "S-<home>") 'smart-home-with-mark) ; Smart home key with selection
(global-set-key (kbd "S-C-a") 'smart-home-with-mark)

(load-library "hideshow")
(global-set-key (kbd "C-+") 'hs-toggle-hiding)
(global-set-key (kbd "C--") 'hs-toggle-hiding)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'scala-mode-hook         'hs-minor-mode)

(global-set-key [f5] (lambda () (interactive)(kill-buffer))) ; kill current buffer with f5

(setq dired-listing-switches "-aghoBG --time-style=+ --group-directories-first") ; dired formatting
(put 'dired-find-alternate-file 'disabled nil) ; use same buffer on 'a'
(add-hook 'dired-mode-hook 'no-linum)

(require 'coffee-mode)

(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))

(require 'git-gutter)
(global-git-gutter-mode t)

(require 'autopair) ; pairs ", {, (, etc
(autopair-global-mode t)

(require 'tramp)
(setq tramp-default-method "ssh") ; use ssh for remote files
(setq tramp-auto-save-directory "/tmp")
(setq tramp-ssh-controlmaster-options "")

(require 'ido)
(ido-mode 0) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Async Shell Command"))
(ido-everywhere 1)
(setq ido-use-faces nil)

(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)
(setq ac-delay 0)
(setq ac-quick-help-delay 0.1)
(global-auto-complete-mode t)

; whitespace stuff
(global-whitespace-mode t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark face))
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [10]) ; newlne
   (tab-mark 9 [8594 9] [92 9]) ; tab
   ))

(require 'scala-mode2)
(require 'sbt-mode)
(modify-coding-system-alist 'file "\\.java$" 'utf-8)

;; Python stuff
;; (require 'ctable)
;; (require 'epc)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:foreground "darkgray")))))

(put 'upcase-region 'disabled nil)
