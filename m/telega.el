;;; telega.el --- telegram client -*- lexical-binding: t; -*-

(use-package telega
  :commands (telega)
  :hook (telega-root-mode . (lambda ()
                              (if (featurep 'evil)
                                  (unevilize-telega)
                                (with-eval-after-load 'evil
                                  (unevilize-telega)))))
  :init
  (defun unevilize-telega ()
    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)
    (evil-set-initial-state 'telega-webpage-mode 'emacs)
    (evil-set-initial-state 'telega-image-mode 'emacs)))
