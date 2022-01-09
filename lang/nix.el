;;; nix.el --- nix setup -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package envrc
  :commands (envrc-mode))
