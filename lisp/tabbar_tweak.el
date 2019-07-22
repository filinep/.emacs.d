;; This are setting for nice tabbar items
;; to have an idea of what it looks like http://imgur.com/b0SNN
;; inspired by Amit Patel screenshot http://www.emacswiki.org/pics/static/NyanModeWithCustomBackground.png

;; Tabbar
(add-to-list 'load-path "~/.emacs.d/tabbar/")
(require 'tabbar)
(tabbar-mwheel-mode)

(global-set-key [C-prior] 'tabbar-backward-tab)
(global-set-key [C-next] 'tabbar-forward-tab)
(global-set-key [C-tab] 'tabbar-forward-tab)
(global-set-key [header-line mouse-4] 'tabbar-backward-tab)
(global-set-key [header-line mouse-5] 'tabbar-forward-tab)
(global-set-key [C-M-prior] 'tabbar-backward-group)
(global-set-key [C-M-next] 'tabbar-forward-group)
(global-set-key [C-M-tab] 'tabbar-forward-group)

(setq tabbar-scroll-right-button nil)
(setq tabbar-scroll-left-button nil)
(setq tabbar-line-separator nil)

;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun my-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*" "*dashboard*" "TAGS"))
     "Common"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq (current-buffer)
           (condition-case nil
               (projectile-buffers-with-file-or-process (projectile-project-buffers))
             (error nil)))
     (projectile-project-name)
     )
    (t
     (if (and (stringp mode-name)
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(tabbar-mode 1)
