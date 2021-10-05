;;; pdf.el --- pdf-tools configuration -*- lexical-binding: t; -*-

(use-package pdf-tools
  :commands (pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1)))
  :custom
  (pdf-view-continuous t)
  (pdf-view-display-size 'fit-height)
  :config
  (p/defpopup "*Outline.*" :side 'right :width 0.3)
  (:maps (:n) pdf-view-mode-map "SPC" #'evil-send-leader
         (:n) pdf-view-mode-map "M-SPC" #'evil-send-localleader))

;;; pdf.el ends here
