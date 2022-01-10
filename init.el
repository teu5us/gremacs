;;; init.el --- Initialize emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Continue loading after early-init

;;; Code:

;;;; load early-init now if it is not supported
(when (version< emacs-version "28.0.50")
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t))

;;;; load core appearance setup
(p/mod core appearance)

;;;; load user's init.el
(p/load-user-config "init.el")

;;;; finish performance configuration
;; the first part is in early-init
(p/hook emacs-startup-hook
        (setq gc-cons-threshold (expt 2 23)
              gc-cons-percentage 0.1
              file-name-handler-alist last-file-name-handler-alist)
        t)
