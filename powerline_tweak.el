(add-to-list 'load-path "~/.emacs.d/nyan-mode/")
(add-to-list 'load-path "~/.emacs.d/powerline/")

(require 'powerline)
(require 'nyan-mode)

(setq nyan-bar-length 16)
(set-face-foreground 'powerline-active1 "white")
(set-face-foreground 'powerline-active2 "black")
(setq powerline-default-separator 'contour)

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face2 'l)
                                     (powerline-buffer-size face2 'l)
                                     (powerline-raw mode-line-mule-info face2 'l)
                                     (powerline-buffer-id face2 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face2 'l))
                                     (powerline-raw " " face2)
                                     (funcall separator-left face2 face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%3l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%2c" face1 'l)
                                     (powerline-raw "%7p" face1 'l)
				     (powerline-raw (nyan-create) face1)
				     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(my-powerline-theme)
