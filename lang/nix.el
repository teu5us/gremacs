;;; nix.el --- nix setup -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (:maps (:i) nix-mode-map "<tab>" #'nix-indent-line))

(use-package envrc
  :commands (envrc-mode))
