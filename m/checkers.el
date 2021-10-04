;;; checkers.el --- Syntax and spellchecking -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up syntax- and spellchecking.

;;; Code:

;;;; flycheck
(use-package flycheck
  :defer t
  :diminish
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))

;;;; flyspell
(use-package flyspell
  :if (executable-find "hunspell")
  :diminish
  :hook
  ((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name (executable-find "hunspell"))
  (ispell-dictionary "ru_RU,en_GB,en_US")
  (ispell-personal-dictionary
   (expand-file-name "personal-dictionary" p/user-dir))
  :config
  (defun ispell-get-coding-system () 'utf-8)
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

;;;;; flyspell extras
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-S-c" . flyspell-correct-wrapper)
              ("C-;" . nil)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)
