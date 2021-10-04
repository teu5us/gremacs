;;; project.el --- Projectile configuration -*- lexical-binding: t; -*-

(use-package projectile
  :after evil
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  (:maps (:n :v) global "<leader>p" #'projectile-command-map))
