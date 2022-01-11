;;; project.el --- Projectile configuration -*- lexical-binding: t; -*-

(use-package projectile
  ;; :after evil
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :custom
  (setq projectile-enable-caching t)
  :config
  (:maps (:n :v) global "<leader>p" #'projectile-command-map))
