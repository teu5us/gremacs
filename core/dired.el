;;; dired.el --- dired configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up the directory editor.

;;; Code:

(use-package dired
  :straight (:type built-in)
  :commands dired
  :custom
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (dired-dwim-target t)
  (load-prefer-newer t)
  (auto-revert-use-notify nil)
  (auto-revert-interval 3)
  (dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  :config
  (global-auto-revert-mode t)
  (put 'dired-find-alternate-file 'disabled t)
  :hook
  (dired-mode . (lambda ()
		  ;; (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
		  ;; (local-set-key (kbd "RET") #'dired-find-alternate-file)
		  (local-set-key (kbd "^")
				 (lambda ()
				   (interactive)
				   (find-alternate-file ".."))))))

;;; dired.el ends here
