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
    (writeroom-mode 'toggle))
;;;;; hooks
  :hook
  (writeroom-mode-disable . (lambda ()
                              (visual-fill-column-mode -1)))
  (prog-mode . (lambda ()
                 (:maps (:n :v) local "<leader>tz" #'p/writeroom)))
  (text-mode . (lambda ()
                 (p/writeroom)
                 (:maps  (:n :v) local "<leader>tz" #'p/writeroom))))
