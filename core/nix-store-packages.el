;;; nix-store-packages.el --- Summary:

;; Retrieve installed epkgs from the nix store.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'straight)

;;;; Utility

(defun nsp/find-string (name lst)
  "Find string NAME in a list of strings LST."
  (cl-position name lst :test #'string-equal))

(defun nsp/pname (str)
  "Extract package name from STR."
  (cl-subseq str 0
             (cl-position ?- str :from-end t)))

(defun nsp/package-from-path (path)
  "Extract package name from store PATH."
  (let* ((split (split-string path "/"))
         (package (nth (+ 2 (nsp/find-string "site-lisp" split)) split)))
    (nsp/pname package)))

(defun nsp/store-path-filter (path)
  "Check if PATH is a store path installed by emacsWithPackages."
  (string-match-p "^/nix/store/[a-z0-9]+-emacs-packages-deps" path))

;;;; Key definitions

(defun nsp/installed-packages ()
  "Return a list of packages installed by nix."
  (cl-remove-duplicates
   (mapcar #'nsp/package-from-path
             (cl-remove-if #'(lambda (path)
                               (or
                                (string-match-p "elpa$" path)
                                (string-match-p "site-lisp$" path)))
                             (cl-remove-if-not #'nsp/store-path-filter load-path)))
   :test #'string-equal))

(defun nsp/override-as-built-in (pname)
  "Override straight recipe for package PNAME with the 'built-in type."
  (let* ((pname-sym (intern pname))
         (recipe (cdr (straight-recipes-retrieve pname-sym))))
    (setf (cl-getf recipe :type) 'built-in)
    (setq recipe (cons pname-sym recipe))
    (straight-override-recipe recipe)))

;; (defun nsp/straight-if-not-in-store (args)
;;   "Advice `straight-use-package' to register package if found in the nix store.
;; Otherwise, call `straight-use-package' with ARGS"
;;   (let* ((melpa-style-recipe (car-safe args))
;;          (recipe (straight--convert-recipe
;;                   (or
;;                    (straight--get-overridden-recipe
;;                     (if (listp melpa-style-recipe)
;;                         (car melpa-style-recipe)
;;                       melpa-style-recipe))
;;                    melpa-style-recipe)
;;                   nil))
;;          (package (cl-getf recipe :package))
;;          (deps (straight--get-dependencies package)))
;;     (when (member package (nsp/installed-packages))
;;       (dolist (p `(,package ,@deps))
;;         (unless (string-equal p "emacs")
;;           (nsp/override-as-built-in p))))
;;     args))

(defun nsp/override-installed ()
  "Override installed packages as 'built-in."
  (dolist (p (nsp/installed-packages))
    (nsp/override-as-built-in p)))

;;;; Set things up

;; (advice-add 'straight-use-package :filter-args #'nsp/straight-if-not-in-store)

(nsp/override-installed)

(provide 'nix-store-packages)
;;; nix-store-packages ends here
