;;; parens.el --- Smartparens configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up parentheses manipulation and highlighting

;;; Code:

;;;; load smartparens
(use-package smartparens
  :diminish smartparens-mode
  :commands (smartparens-mode smartparens-strict-mode)
  :init
  (defun p/lisp? ()
    (member major-mode '(emacs-lisp-mode
                         lisp-mode
                         scheme-mode
                         hy-mode
                         lfe-mode
                         clojure-mode
                         sly-mrepl-mode)))
;;;;; config
  :config
  (p/require 'smartparens 'smartparens-config)
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
;;;;;; define a hydra for lispy keys
  (defhydra hydra-sp (:pre (hydra-set-property 'hydra-sp :verbosity 0)
                      :color red
                      :foreign-keys warn
                      :exit nil)
    "

                                   --- LISP StructEd ---
----------------------------------------------------------------------------------------

"
    ("a" sp-beginning-of-sexp "beginning of sexp" :column "Movement")
    ("d" sp-kill-symbol "kill sym")
    ("D" sp-backward-kill-symbol "backward kill sym")
    ("e" sp-end-of-sexp "end of sexp")
    ("f" sp-forward-symbol "forward sym")
    ("b" sp-backward-symbol "backward sym")
    ("n" sp-forward-sexp "forward sexp")
    ("p" sp-backward-sexp "backward sexp")
    ("o" sp-up-sexp "up sexp")
    ("O" sp-backward-up-sexp "backward down sexp")
    ("I" sp-down-sexp "up sexp")
    ("i" sp-backward-down-sexp "backward down sexp")

    ("cc" sp-clone-sexp "clone" :column "Actions")
    ("ci" sp-change-inner "change inner")
    ("ce" sp-change-enclosing "change enclosing")
    ("xa" sp-absorb-sexp "absorb")
    ("xe" sp-emit-sexp "emit")
    ("xE" eval-last-sexp "eval")
    ("xj" sp-join-sexp "join")
    ("xr" sp-raise-sexp "raise")
    ("xs" sp-split-sexp "split")
    ("x/" sp-splice-sexp "splice")
    (";" sp-comment "comment")
    ("<tab>" indent-sexp "indent sexp")

    ("h" sp-slurp-hybrid-sexp "slurp hybrid" :exit t :column "Slurp/Barf")
    ("]" sp-forward-slurp-sexp "slurp forward")
    ("}" sp-forward-barf-sexp "barf forward")
    ("[" sp-backward-slurp-sexp "slurp backward")
    ("{" sp-backward-barf-sexp "barf backward")
    ("(" sp-kill-sexp "kill sexp")
    (")" sp-backward-kill-sexp "backward kill sexp")

    ("?" (hydra-set-property 'hydra-sp :verbosity 2) "help" :column "Etc")
    (":" execute-extended-command)
    ("q" nil "quit" :color blue)
    ("<escape>" nil "quit" :color blue))
;;;;;; highlight matching parens
  (when show-paren-mode
    (show-paren-mode -1))
  (show-smartparens-global-mode)
;;;;; customs
  :custom
  (sp-show-pair-delay 0)
;;;;; hooks
  :hook
  ((text-mode org-mode) . smartparens-mode)
  ((sly-mrepl-mode prog-mode)
   .
   (lambda ()
     (smartparens-strict-mode)
     (if (p/lisp?)
         (progn
           (local-set-key (kbd "M-z") #'hydra-sp/body)
           (p/load-lispy-parens-bindings)
           (setq-local evil-move-beyond-eol t))
       (:maps (:n :v :i) local "M-w" #'sp-slurp-hybrid-sexp)))))
