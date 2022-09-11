;;; eshell.el --- eshell configuration -*- lexical-binding : t; -*-

(use-package eshell
  :config
  (require 'em-smart)
  (require 'esh-module)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :hook (eshell-mode . eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  (eshell-aliases-file (expand-file-name "eshell-aliases" p/user-dir)))

(use-package aweshell
  :straight '(aweshell :type git
                       :host github
                       :repo "manateelazycat/aweshell")
  :commands (aweshell-new aweshell-dedicated-toggle)
  :bind (("M-#" . aweshell-dedicated-toggle)
         (:map evil-normal-state-map ("<leader>oe" . aweshell-dedicated-toggle))
         (:map evil-visual-state-map ("<leader>oe" . aweshell-dedicated-toggle))
         (:map eshell-mode-map ("C-d" . eshell-life-is-too-much)))
  :init
  (:maps (:n :v :i) eshell-mode-map "C-d" #'eshell-life-is-too-much
         (:n :v :i) global "<leader>oe" #'aweshell-dedicated-toggle)
  :config
  (defun p/aweshell-dedicated-kill ()
    (interactive)
    (let ((buf (buffer-file-name)))
      (when (buffer-live-p aweshell-dedicated-buffer)
        (unless (string-equal (or (when buf (file-name-directory buf))
                                  default-directory)
                              (substring (buffer-name aweshell-dedicated-buffer) 10))
          (kill-buffer aweshell-dedicated-buffer)))))
  (advice-add #'aweshell-dedicated-open :before #'p/aweshell-dedicated-kill)
  (push "\\Aweshell:" special-display-regexps))
