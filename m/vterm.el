;;; vterm.el --- vterm configuration -*- lexical-binding: t; -*-

(use-package vterm
  :commands (vterm)
  :bind
  ("<leader>ot" . vterm-popup)
  ("<leader>oT" . vterm)
  (:map vterm-mode-map ("C-d" . vterm-send-C-d))
  :init
  (defun vterm-popup ()
    (interactive)
    (defvar vterm-buffer-name)
    (let ((vterm-buffer-name "*vterm-popup*"))
      (vterm)))
  (p/defpopup "*vterm-popup*" t :side 'bottom :height 0.3)
  :config
  (:maps (:i) vterm-mode-map "C-d" #'vterm-send-C-d)
  :custom
  (vterm-max-scrollback 5000)
  (vterm-always-compile-module t)
  (vterm-eval-cmds nil))
