;;; snippets.el --- Snippets configuration -*- lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs `(,(expand-file-name "snippets" p/user-dir)))
  (yas-wrap-around-region t)
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)
