;;; haskell.el --- haskell config -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  :bind (:map haskell-mode-map ("C-c C-z" . haskell-interactive-bring))
  :config
  (ignore-errors
    (require 'haskell-doc)
    (require 'haskell-decl-scan))
  (when (find-library-name "haskell-doc")
    (setq-default haskell-doc-show-global-types t
                  haskell-doc-show-prelude t
                  haskell-doc-show-reserved t
                  haskell-doc-show-strategy t
                  haskell-doc-show-user-defined t)
    (add-hook 'haskell-mode-hook #'haskell-doc-mode)))

(use-package lsp-haskell
  :hook (haskell-mode . lsp))
