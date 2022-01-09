;;; cl.el --- common-lisp setup -*- lexical-binding: t; -*-

(use-package sly
  :commands (sly)
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :preface
  (add-to-list 'display-buffer-alist
               '("sly-mrepl*"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height . 0.3)))
  (add-to-list 'display-buffer-alist
               '("\\*sly-db*"
                 (display-buffer-in-previous-window)
                 (inhibit-same-window . t)))
  ;; (p/defpopup "*sly-mrepl.*" t :side 'bottom :height 0.3)
  :custom
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" p/user-dir))
  (sly-mrepl-prevent-duplicate-history t)
  (sly-autodoc-use-multiline-p t)
  :config
  (p/require 'sly 'sly-autoloads))

(use-package sly-asdf
  :after sly)

(use-package sly-macrostep
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package sly-repl-ansi-color
  :after sly)

(use-package sly-named-readtables
  :after sly)
