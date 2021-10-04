;;; telega.el --- telegram client -*- lexical-binding: t; -*-

(use-package telega
  :commands (telega)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)
    (evil-set-initial-state 'telega-webpage-mode 'emacs)
    (evil-set-initial-state 'telega-image-mode 'emacs)))
