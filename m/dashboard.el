;;; dashboard.el --- Dashboard configuration -*- lexical-binding: t; -*-

;;;; page-break-lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;;;; dashboard
(use-package dashboard
  :diminish dashboard-mode
  :hook
  (after-init . (lambda ()
                  (dashboard-setup-startup-hook)
                  (advice-add #'find-file :after #'p/dashboard-refresh-buffer)))
  :config
  (:map (:n :v) dashboard-mode-map "x" #'dashboard-remove-item-under)
  (push "*dashboard*" p/buffer-predicate-names)
;;;;; automatically refresh dashboard in the background (see hook above)
  (defun p/dashboard-refresh-buffer (&rest args)
    "Refresh dashboard buffer without switching to it."
    (defvar dahsboard-force-refresh)
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
