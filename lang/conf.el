;;; conf.el --- conf/ini/properties -*- lexical-binding: t; -*-

(use-package conf-mode
  :commands conf-mode
  :mode (("\\.conf\\'" . conf-mode)
         ("\\.ini\\'" . conf-mode)))
