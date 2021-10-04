;;; package-management.el --- Setup core package management -*- lexical-binding: t; -*-

;; Install straight.el
(defvar bootstrap-version)

(defvar straight-build-cache-fixed-name (format "build-%s-cache.el" emacs-version))
(defvar straight-check-for-modifications
  (if (executable-find "watchexec")
      '(watch-files find-when-checking)
    nil))
(defvar straight-use-package-by-default t)
(defvar straight-repository-branch "develop")
(defvar straight-build-dir (format "build-%s" emacs-version))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

(use-package git)
