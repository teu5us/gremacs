;;; lsp.el --- lsp-mode configuration -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp--find-clients)
  :hook (find-file . p/lsp-deferred-unless-client)
  :custom
  (lsp-keymap-prefix "<leader>c")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-hook nil)
  (lsp-headerline-breadcrumb-enable nil)
  :bind (:map lsp-mode-map ("F" . lsp-format-buffer))
  :init
  (defun p/lsp-deferred-unless-client ()
    (when (lsp--find-clients)
      (lsp-deferred)))
  :config
  (defun p/lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))

;; Note: lsp-ui-doc is too annoying, so it will not be triggered upon
;; hovering. You have to toggle it using M-i.
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))
