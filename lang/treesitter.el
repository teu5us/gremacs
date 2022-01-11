;;; treesitter.el --- treesitter support -*- lexical-binding: t; -*-

(use-package tree-sitter
  :diminish
  :hook
  ((python-mode rust-mode) . tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;; treesitter.el ends here
