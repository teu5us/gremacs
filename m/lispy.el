;;; lispy.el --- Lispy configuration -*- lexical-binding: t; -*-

(use-package lispy
  :diminish
  :init
  (setq lispy-completion-method 'default)
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (hy-mode . lispy-mode)
  :custom
  (lispy-close-quotes-at-end-p t))

(use-package lispyville
  :after (lispy evil)
  :diminish
  :hook
  (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme '((operators normal)
			       c-w
			       (prettify insert)
			       (atom-movement t)
			       slurp/barf-lispy
			       additional
			       additional-insert))
  :config
  (lispyville-set-key-theme)
  (:i lispyville-mode-map "C-<return>" #'lispy-out-forward-newline)
  (:i lispyville-mode-map "C-e" #'lispy-right)
  (:i lispyville-mode-map "<backspace>" #'lispy-delete-backward)
  (:i lispyville-mode-map ";" #'lispy-comment))
