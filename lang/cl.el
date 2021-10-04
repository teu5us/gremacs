;;; cl.el --- common-lisp setup -*- lexical-binding: t; -*-

(use-package sly
  :commands (sly)
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :custom
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" p/user-dir))
  (sly-mrepl-prevent-duplicate-history t)
  (sly-autodoc-use-multiline-p t)
  :config
  (require 'sly-autoloads)
  (p/define-popup 'regexp "^*sly-mrepl.*"))

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
