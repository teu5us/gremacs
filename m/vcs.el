;;; vcs.el --- Git configuration -*- lexical-binding: t; -*-

(use-package magit
  ;; :commands (magit-status)
  :defer 1
  :config
  (:maps :a global "<leader>gg" #'magit-status))
