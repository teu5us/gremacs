;;; haskell.el --- haskell config -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . haskell-doc-mode)
  :bind (:map haskell-mode-map ("C-c C-z" . haskell-interactive-bring)))

(use-package lsp-haskell
  :hook (haskell-mode . lsp))
