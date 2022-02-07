;;; cl.el --- common-lisp setup -*- lexical-binding: t; -*-

(use-package sly
  :commands (sly)
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :preface
  (add-to-list 'display-buffer-alist
               '("^\\*sly-\\(?:compilation\\|traces\\)*"
                 (display-buffer-reuse-mode-window
                  display-buffer-in-previous-window
                  display-buffer-pop-up-window)
                 (inhibit-switch-frame . t)
                 (reusable-frames . nil)
                 (window . root)
                 (direction . below)
                 (window-height . 0.25)))
  (add-to-list 'display-buffer-alist
               '("sly-mrepl*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (inhibit-switch-frame . t)
                 (window-height . 0.3)
                 (direction . below)
                 (popup . t)
                 (dedicated . t)))
  (push "\\*sly-db*" p/buffer-predicate-names)
  (push "\\*sly-compilation*" p/buffer-predicate-names)
  (push "\\*sly-traces*" p/buffer-predicate-names)
  (push "\\*sly-inspector*" p/buffer-predicate-names)
  :custom
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" p/user-dir))
  (sly-mrepl-prevent-duplicate-history t)
  (sly-autodoc-use-multiline-p t)
  :config
  (require 'sly-autoloads))

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
