;;; evil.el --- Snippets configuration -*- lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" p/user-dir))))

(use-package yasnippet-snippets
  :after yasnippet)
