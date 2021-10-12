;;; rainbow.el --- Highlighting-stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up color markers, parentheses, indent guides, current line
;; and diff highlighting.

;;; Code:

(use-package rainbow-mode
  :diminish
  :commands (rainbow-mode)
  :config
  (:maps (:n :v) global "<leader>tC" #'rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character))

(use-package hl-line
  :hook
  (after-init . global-hl-line-mode))

(use-package diff-hl
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :config
  (when (boundp 'magit-pre-refresh-hook)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh))
  (when (boundp 'magit-post-refresh-hook)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))
