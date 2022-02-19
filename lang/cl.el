;;; cl.el --- common-lisp setup -*- lexical-binding: t; -*-

(use-package sly
  :commands (sly)
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :preface
  (p/defpopups `(("^\\*sly-mrepl.*" :side bottom :height 0.3 :override-quit nil)
                 ("^\\*sly-compilation.*" :side left :width 0.5)
                 ("^\\*sly-traces.*" :side left :width 0.5)
                 ("^\\*sly-db.*" :side right :width 0.5)))
  :custom
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" p/user-dir))
  (sly-mrepl-prevent-duplicate-history t)
  (sly-autodoc-use-multiline-p t)
  :config
  (ignore-errors
    (require 'sly-autoloads)))

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
