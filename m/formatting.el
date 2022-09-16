;;; formatting.el --- Format-all -*- lexical-binding: t; -*-

(use-package format-all
  :demand t
  :hook (prog-mode . format-all-buffer)
  :bind ("<leader>cf" . format-all-buffer))
