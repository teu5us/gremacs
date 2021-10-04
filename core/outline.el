;;; outline.el --- configure outshine-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; This module loads outshine to extend outline mode.
;; By default, outshine is only enabled in `emacs-lisp-mode'.

;;; Code:

(use-package outshine
  :diminish (outline-mode
             outshine-mode
             outline-minor-mode
             outorg-edit-minor-mode)
  :bind (:map outorg-edit-minor-mode-map
              ("M-#" . nil)
              ("C-c C-c" . #'outorg-copy-edits-and-exit))
  :custom
  (outshine-use-speed-commands t)
  (outshine-startup-folded-p t)
  :hook (emacs-lisp-mode . outshine-mode))

;;; outline.el ends here
