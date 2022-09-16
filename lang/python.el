;;; python.el --- python config -*- lexical-binding: t; -*-

(use-package python-mode
  :mode "\\.py\\'"
  :bind (:map python-mode-map
              ("<backspace>" . backward-delete-char)
              ([backspace] . backward-delete-char))
  :custom
  (python-shell-interpreter "jupyter-console")
  (python-shell-interpreter-args "--single-prompt")
  (python-shell-prompt-detect-failure-warnings nil)
  (python-indent-offset 4)
  (flycheck-python-pylint-executable "pylint")
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3")
  :init
  (add-hook 'python-mode-hook
            #'(lambda ()
                (setq-local lsp-diagnostics-provider :none)))
  :config
  (add-to-list
   'python-shell-completion-native-disabled-interpreters "jupyter-console")
  (add-to-list
   'python-shell-completion-native-disabled-interpreters "jupyter"))

(use-package lsp-pyright
  :after lsp-mode
  :config
  (require 'lsp-pyright)
  :custom
  (lsp-pyright-multi-root nil)
  (lsp-pyright-diagnostic-mode "workspace"))

(use-package pyvenv
  :commands (pyvenv-activate
             pyvenv-deactivate
             pyvenv-workon)
  :init
  (defvar venv-alist
    '(("learning" . "~/.virtualenvs/learning")))

  (defun choose-venv (deactivate)
    (interactive "P")
    (if deactivate
        (pyvenv-deactivate)
      (let ((venv-name (completing-read "Choose python-venv: " venv-alist nil t)))
        (pyvenv-activate (cdr (assoc-string venv-name venv-alist)))))))

(use-package py-isort
  :after python-mode
  :hook (python-mode . py-isort-enable-on-save))
