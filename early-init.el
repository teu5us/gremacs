;;; early-init.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:

;; Start early Emacs setup

;;; Code:

;;;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;;;; make warnings more friendly
(setq warning-minimum-level :error)

;;;; disable site-run-file
(setq site-run-file nil)

;;;; speed optimization
;; finished in init.el
(defvar last-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;;;; load core configuration
(load (expand-file-name "core/core.el" user-emacs-directory) nil t)

;;;; load user's early-init
(p/load-user-config "early-init.el")

;;;; load native-comp settings
(p/mod core comp)
