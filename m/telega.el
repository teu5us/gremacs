;;; telega.el --- telegram client -*- lexical-binding: t; -*-

(use-package telega
  :commands (telega)
  :diminish telega-root-auto-fill-mode
  :hook (telega-root-mode . (lambda ()
                              (if (featurep 'evil)
                                  (unevilize-telega)
                                (with-eval-after-load 'evil
                                  (unevilize-telega)))))
  :init
  (dolist (m '(telega-root-mode
               telega-chat-mode
               telega-webpage-mode
               telega-image-mode))
    (push m boon-special-mode-list))
  (defun unevilize-telega ()
    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)
    (evil-set-initial-state 'telega-webpage-mode 'emacs)
    (evil-set-initial-state 'telega-image-mode 'emacs)))
