;;; haskell.el --- haskell config -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map ("C-c C-z" . haskell-interactive-bring)))
