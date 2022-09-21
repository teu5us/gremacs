;;; web.el --- web setup -*- lexical-binding: t; -*-

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 4))

(use-package emmet-mode
  :after web-mode
  :commands (emmet-expand-line)
  :hook (web-mode . (lambda ()
                      (:maps (:n :i) local "<localleader>e" #'emmet-expand-line))))
