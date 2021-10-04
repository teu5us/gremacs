;;; formatting.el --- Format-all -*- lexical-binding: t; -*-

(use-package format-all
  :config
  (:maps (:n :v) global "<leader>cf" #'format-all-buffer))
