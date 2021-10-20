;;; vcs.el --- Git configuration -*- lexical-binding: t; -*-

(use-package magit
  ;; :commands (magit-status)
  :defer 1
  :config
  (defun p/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (evil-define-key '(normal visual) magit-status-mode-map "q" #'p/magit-kill-buffers)
  (:maps :a global "<leader>gg" #'magit-status))
