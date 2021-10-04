;;; writeroom.el --- writeroom configuration -*- lexical-binding: t; -*-

;;;; load writeroom
(use-package writeroom-mode
  :after visual-fill-column
  :commands (writeroom-mode)
;;;;; customs
  :custom
  (writeroom-global-effects nil)
  (writeroom-mode-line t)
  (writeroom-header-line t)
  (writeroom-maximize-window nil)
  (writeroom-width 80)
;;;;; hooks
  :hook
  (writeroom-mode-disable . (lambda ()
                              (visual-fill-column-mode -1)))
  (prog-mode . (lambda ()
                 (:maps (:n :v) local "<leader>tz" #'writeroom-mode)))
  (text-mode . (lambda ()
                 (writeroom-mode)
                 (:maps  (:n :v) local "<leader>tz" #'writeroom-mode))))
