;;; vcs.el --- Git configuration -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status)
  :init
  (:maps :a global "<leader>gg" #'magit-status)
  :config
  (defun p/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (:map (:n :v) magit-status-mode-map "q" #'p/magit-kill-buffers))
