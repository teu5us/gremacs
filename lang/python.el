;;; python.el --- python config -*- lexical-binding: t; -*-

(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :bind (:map python-mode-map
              ("<backspace>" . backward-delete-char)
              ([backspace] . backward-delete-char))
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :after lsp-mode
  :config
  (require 'lsp-pyright)
  :custom
  (lsp-pyright-multi-root nil))

(use-package pyvenv
  :config
  (defvar venv-alist
    '(("learning" . "~/.virtualenvs/learning")))

  (defun choose-venv (deactivate)
    (interactive "P")
    (if deactivate
        (pyvenv-deactivate)
      (let ((venv-name (completing-read "Choose python-venv: " venv-alist nil t)))
        (pyvenv-activate (cdr (assoc-string venv-name venv-alist)))))))
