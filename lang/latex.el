;;; latex.el --- latex configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file configures latex.

;;; Code:

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode))

(use-package auctex-latexmk
  :after auctex
  :config
  (auctex-latexmk-setup))
