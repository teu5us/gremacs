;;; core.el --- Main core file -*- lexical-binding: t; -*-

;;; Commentary:

;; This main core module defines core settings itself and loads further core
;; modules.

;;; Code:

;;;; coding system
(defun p/setup-coding-system ()
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)
    (set-language-environment "UTF-8")
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8))
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))
(add-hook 'after-init-hook #'p/setup-coding-system)

;;;; module system
(defun p/all-p (pred lst)
  "Check if all elements in the list LST satisfy predicate PRED."
  (cl-loop for el in (mapcar pred lst)
           when (null el)
           do (cl-return el)
           finally (return t)))

(defun p/module-name-concat (&rest strings)
  "Concatenate module names."
  (require 'cl-lib)
  (if (fboundp 'file-name-concat)
      (apply #'file-name-concat strings)
    (let ((slashes (make-list (1- (length strings))
                              (if (string-equal system-type "windows-nt")
                                  "\\" "/"))))
      (apply #'concat (car strings)
              (cl-loop for str in (cdr strings)
                       for sl in slashes
                       collect (concat sl str))))))

(defmacro p/mod (&rest names)
  "Load a submodule (built from NAMES using `p/module-name-concat')
from `user-emacs-directory' or subfolder."
  (require 'cl-lib)
  (if (p/all-p #'symbolp names)
      (let ((base-dir (directory-file-name
                       (if (eql 'l (car names))
                           (file-name-directory (or load-file-name buffer-file-name))
                         user-emacs-directory)))
            (names-internal (if (eql 'l (car names))
                       (cdr names)
                     names)))
        `(load
          (concat
           (apply #'p/module-name-concat
                  ,base-dir
                  (cl-loop for s in ',names-internal
                           collect (symbol-name s)))
           ".el")
          nil t))
    (error "`NAMES' must be a list of symbols")))

(defmacro p/mods (&rest mods)
  "Bulk load modules using `p/mod'."
  (require 'cl-lib)
  `(progn
     ,@(cl-loop for mod in mods
                collect (cons 'p/mod mod))))

;;;; shorten hook functionality
(defmacro p/hook (place action &optional depth local)
  "A shorter `add-hook'.
Allows something like `(p/hook emacs-startup-hook (setq a 1))'."
  (if (eql 'lambda (car action))
      `(add-hook ',place #',action ,depth ,local)
    `(add-hook ',place #'(lambda () ,action) ,depth ,local)))

;;;; configuration loading
(defvar p/user-dir (or (getenv "GROUND_EMACS_USERDIR")
                       (expand-file-name "~/.gremacs.d"))
  "User's personal configuration directory.

Can be set using GROUND_EMACS_USERDIR environment variable.")

(defun p/load-user-config-from-directory (config-name directory)
  "Load user configuration from specified DIRECTORY."
  (let* ((conf-path (expand-file-name config-name directory)))
    (unless (file-exists-p conf-path)
      (with-temp-buffer (write-file conf-path)))
    (load conf-path nil t)))

(defun p/load-user-config (config-name)
  "Load user configuration."
  (if (file-name-absolute-p p/user-dir)
      (progn
        (unless (file-directory-p p/user-dir)
          (make-directory p/user-dir))
        (p/load-user-config-from-directory config-name p/user-dir))
    (error "USER-DIR must be an absolute path:
check the value of GROUND_EMACS_USERDIR environment variable")))

;;;; one space for sentence end
(setq sentence-end-double-space nil)

;;;; history
;; Set history-length longer
(setq-default history-length 500)
;; Save history
(setq savehist-file (expand-file-name "history" p/user-dir))
(add-hook 'after-init-hook #'savehist-mode)

;;;; recent files
;; Save recent files
(setq recentf-max-saved-items 200)
(add-hook 'after-init-hook #'recentf-mode)

;;;; disable autsaving
(setq auto-save-default nil)

;;;; save all `custom-set-variables' in a separate file
(setq custom-file (expand-file-name "custom.el" p/user-dir))

;;;; allow calling minibuf from minibuf
(setq enable-recursive-minibuffers t)

;;;; no backup files
(setq make-backup-files nil)

;;;; make completion more flexible
(setq completion-styles '(basic flex))

;;;; highlight matching parens
;; disabled by m/parens if it is enabled
(show-paren-mode 1)

;;;; confirm killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;;;; no lock files
(setq-default create-lockfiles nil)

;;;; better compilation buffer
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;;;; so-long mode for performance
;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (add-hook 'after-init-hook #'global-so-long-mode))

;;;; add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;;;; save position in buffer
(save-place-mode 1)

;;;; replace selection on insert
(delete-selection-mode 1)

;;;; map Alt key to Meta
(setq x-alt-keysym 'meta)

;;;; remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;;;; native-comp march custom
(defcustom p/native-comp-march nil
  "March to be used in `native-comp-compiler-options'."
  :type 'string)

;;;; load other core modules
(p/mod l dired)
(p/mod l sudo)
(p/mod l indentation)
(p/mod l windows)
(p/mod l outline)
(p/mod l helpful)

;;;; define some useful functions to map later
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer nil t))

(defun find-file-user-dir ()
  (interactive)
  (let ((default-directory (concat p/user-dir "/")))
    (call-interactively #'find-file)))

(defun find-file-conf-dir ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively #'find-file)))

;;;; load hydra
(use-package hydra
  :commands (defhydra)
  :config
  (defhydra hydra-tab-bar ()
    "Tab Bar Operations"
    ("t" tab-new "Create a new tab" :column "Creation")
    ("d" dired-other-tab "Open Dired in another tab")
    ("f" find-file-other-tab "Find file in another tab")
    ("x" tab-close "Close current tab")
    ("m" tab-move "Move current tab" :column "Management")
    ("R" tab-rename "Rename Tab")
    ("h" tab-bar-mode "Show/Hide Tab Bar" :exit t)
    ("<return>" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
    ("r" tab-recent "Recent Tab" :exit t)
    ("n" tab-next "Next Tab")
    ("p" tab-previous "Previous Tab")
    ("q" nil "Exit" :column "Quit" :exit t)
    ("<escape>" nil "Exit" :exit t))
  (global-set-key (kbd "C-x t") #'hydra-tab-bar/body))

;;; core.el ends here
