;; Load paths and files
(add-to-list 'load-path "~/.emacs.d/lisp")

(mapc (lambda (x) (add-to-list 'load-path (concat "~/.emacs.d/" x "/"))) 
      '("popup" "deferred"
        "scala-mode2" "sbt-mode" 
        "s" "neotree" "projectile"
        "git-gutter" "async" "helm"
        "epl" "pkg-info.el" "f.el" "dash.el"
        "company-mode" "helmprojectile"
        "jade-mode" "coffee-mode"))

(mapc 'load
      '("custom_functions" 
        "powerline_tweak" 
        "tabbar_tweak"
        ))

;; Clipboard
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

;; Stuff
(setq inhibit-startup-screen t) ; No startup screen
(load-theme 'tango-dark) ; Sets colour theme
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; No scrollbar
(menu-bar-mode -99) ; No menubar
(delete-selection-mode 1) ; Delete selected text when typing
(show-paren-mode 1) ; Matches parentheses
(global-subword-mode) ; Navigate camel case
(global-hl-line-mode 1) ; Highlights current line
(set-face-background 'hl-line "#3e4446") ; Set colour of highlighted line
(set-face-foreground 'highlight nil) ; Keep syntax highlighting on current line
(set-default-font "DejaVu Sans Mono-13") ; Set font
(setq-default cursor-type 'bar) ; Use bar for cursor
(setq scroll-error-top-bottom t) ; Allows pgup/pgdn to go to extremes
(setq next-screen-context-line 3) ; 3 line overlap on page scroll
(setq c-default-style "linux" c-basic-offset 4) ; Supposed to fix indentation
(electric-pair-mode) ; pairs ", {, (, etc
(global-set-key [f5] (lambda () (interactive)(kill-buffer))) ; kill current buffer with f5

(require 'company)
(add-hook 'after-init-hook 'global-company-mode-mode)
(global-set-key (kbd "C-<return>") 'company-complete)

;; Smart home key
(global-set-key [home] 'smart-home) ; Smart home key
(global-set-key (kbd "C-a") 'smart-home)
(global-set-key (kbd "S-<home>") 'smart-home-with-mark) ; Smart home key with selection
(global-set-key (kbd "S-C-a") 'smart-home-with-mark)

;; Hide show
(load-library "hideshow")
(global-set-key (kbd "C-+") 'hs-toggle-hiding)
(global-set-key (kbd "C--") 'hs-toggle-hiding)
(defun hideshow-ignore-setup-failure() (ignore-errors (hs-minor-mode)))
(define-globalized-minor-mode global-hs-minor-mode hs-minor-mode hideshow-ignore-setup-failure)
(global-hs-minor-mode 1)

(require 'projectile)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-globally-ignored-file-suffixes '(".jar"))

(require 'neotree)
(setq neo-smart-open nil)
(setq neo-project-root-open t)
(setq neo-show-hidden-files t)
(setq neo-window-width 35)
(global-set-key [f8] 'neotree-project-root-dir-or-current-dir)
(setq projectile-switch-project-action 'neotree-projectile-action)

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(setq
 ;helm-split-window-in-side-p          t ; open helm buffer inside current window, not occupy whole other window
     helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
     helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
     helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
     helm-ff-file-name-history-use-recentf t
     helm-echo-input-in-header-line        t
     helm-autoresize-max-height            50
     helm-autoresize-min-height            50
     helm-split-window-default-side        'right)
(when (executable-find "ack-grep")
 (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
       helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))
(helm-mode 1)

(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'coffee-mode)
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))

(require 'git-gutter)
(global-git-gutter-mode t)

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

;; Scala/SBT stuff
(require 'scala-mode2)
(require 'sbt-mode)
(modify-coding-system-alist 'file "\\.java$" 'utf-8)
;;(add-hook 'sbt-mode-hook
;;          (lambda ()
;;            (setq prettify-symbols-alist
;;                  `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
;;                    (,(expand-file-name "~") . ?~)))
;;            (prettify-symbols-mode t)))
;;(global-set-key [f12] '(lambda () (interactive) (sbt-command "compile")))

;; Python stuff
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)
             (setq js-indent-level 2)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight light :height 160 :width normal))))
 '(whitespace-space ((t (:foreground "darkgray")))))

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil))
