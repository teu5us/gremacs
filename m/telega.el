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
  (defun unevilize-telega ()
    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)
    (evil-set-initial-state 'telega-webpage-mode 'emacs)
    (evil-set-initial-state 'telega-image-mode 'emacs))
  (with-eval-after-load 'boon
    (dolist (m '(telega-root-mode
                 telega-chat-mode
                 telega-webpage-mode
                 telega-image-mode))
      (push m boon-special-mode-list)))
  (defun p/telega-server--find-bin ()
    "Find telega-server executable.
Raise error if not found."
    (let ((exec-path (cons telega-directory exec-path)))
      (or (executable-find "telega-server")
          (executable-find telega-server-command)
          (progn (telega-server-build)
                 (executable-find "telega-server"))
          (error "`telega-server' not found in exec-path"))))
  (advice-add 'telega-server--find-bin :override #'p/telega-server--find-bin))
