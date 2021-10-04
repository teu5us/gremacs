;;; helpful.el --- Enhance help-mode -*- lexical-binding: t; -*-

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-symbol)
  :bind (:map help-map ("C-d" . helpful-at-point))
  :init
  (advice-add #'describe-symbol :override #'helpful-symbol)
  (advice-add #'describe-function :override #'helpful-callable)
  (advice-add #'describe-variable :override #'helpful-variable)
  (advice-add #'describe-key :override #'helpful-key))
