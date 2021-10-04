;;; dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-

;;;; page-break-lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;;;; dashboard
(use-package dashboard
  :diminish dashboard-mode
  :hook
  (after-init . dashboard-setup-startup-hook)
  (kill-buffer . p/dashboard-refresh-buffer)
  :config
  (push "*dashboard*" p/buffer-predicate-names)
;;;;; automatically refresh dashboard in the background (see kill-buffer hook above)
  (defun p/dashboard-refresh-buffer ()
    "Refresh dashboard buffer without switching to it."
    (let ((dashboard-force-refresh t))
      (save-current-buffer
        (set-buffer (get-buffer-create dashboard-buffer-name))
        (dashboard-insert-startupify-lists))))
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 10)
                     (projects . 5)
                     (bookmarks . 5))))
