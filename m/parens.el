;;; parens.el --- Smartparens configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up parentheses manipulation and highlighting

;;; Code:

;;;; load smartparens
(use-package smartparens
  :diminish smartparens-mode
;;;;; config
  :config
  (require 'smartparens-config)
;;;;;; define motions for lispy modes
  (defun p/load-lispy-parens-bindings ()
    (:maps (:i) local "M-<backspace>" #'sp-backward-kill-symbol
           (:i) local "M-D" #'sp-kill-symbol
           (:i) local "M-(" #'sp-kill-sexp
           (:i) local "M-)" #'sp-backward-kill-sexp
           (:i) local ";" #'sp-comment
           (:n :i) local "M-c" #'sp-clone-sexp
           (:n :i) local "M-r" #'sp-raise-sexp
           (:n :i) local "M-e" #'sp-emit-sexp
           (:n :i) local "M-S" #'sp-split-sexp
           (:n :i) local "M-j" #'sp-join-sexp
           (:n :i) local "M-a" #'sp-absorb-sexp
           (:n :v :i) local "M-f" #'sp-forward-symbol
           (:n :v :i) local "M-b" #'sp-backward-symbol
           (:n :v :i) local "M-n" #'sp-forward-sexp
           (:n :v :i) local "M-p" #'sp-backward-sexp
           (:n :v :i) local "M-H" #'sp-down-sexp
           (:n :v :i) local "M-h" #'sp-backward-down-sexp
           (:n :v :i) local "M-l" #'sp-up-sexp
           (:n :v :i) local "M-L" #'sp-backward-up-sexp
           (:n :v :i) local "M-]" #'sp-forward-slurp-sexp
           (:n :v :i) local "M-}" #'sp-forward-barf-sexp
           (:n :v :i) local "M-[" #'sp-backward-slurp-sexp
           (:n :v :i) local "M-{" #'sp-backward-barf-sexp
           (:n :v :i) local "M-/" #'sp-splice-sexp))
;;;;;; highlight matching parens
  (when show-paren-mode
    (show-paren-mode -1))
  (show-smartparens-global-mode)
;;;;; customs
  :custom
  (sp-show-pair-delay 0)
;;;;; hooks
  :hook
  ((sly-mrepl-mode prog-mode)
   .
   (lambda ()
     (smartparens-strict-mode)
     (if (or (derived-mode-p 'lisp-data-mode)
             (member major-mode '(scheme-mode
                                  hy-mode
                                  lfe-mode
                                  clojure-mode
                                  sly-mrepl-mode)))
         (progn
           (p/load-lispy-parens-bindings)
           (set (make-local-variable 'evil-move-beyond-eol) t))
       (:maps (:n :v :i) local "M-w" #'sp-slurp-hybrid-sexp)))))
