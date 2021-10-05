;;; treesitter.el --- treesitter support -*- lexical-binding: t; -*-

(use-package tree-sitter
  :diminish
  :hook
  (after-init . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;; treesitter.el ends here
