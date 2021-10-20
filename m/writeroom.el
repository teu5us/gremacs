;;; writeroom.el --- writeroom configuration -*- lexical-binding: t; -*-

;;;; load writeroom
(use-package writeroom-mode
  :commands (writeroom-mode)
;;;;; customs
  :custom
  (writeroom-global-effects nil)
  (writeroom-mode-line t)
  (writeroom-header-line t)
  (writeroom-maximize-window nil)
  (writeroom-width 80)
  :init
  (defun p/writeroom ()
    (interactive)
    (if (derived-mode-p 'special-mode)
        (message "Cannot enable writeroom in special buffer.")
      (writeroom-mode 'toggle)))
  :bind
  ("<leader>tz" . p/writeroom)
;;;;; hooks
  :hook
  (writeroom-mode-disable . (lambda ()
                              (visual-fill-column-mode -1)))
  (text-mode . p/writeroom))
