;;; rust.el --- rust config -*- lexical-binding : t; -*-

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run)))

(use-package flycheck-rust
  :after flycheck
  :hook (flycheck-mode . flycheck-rust-setup))
