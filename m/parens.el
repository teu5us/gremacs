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
    (:maps (:n :i) local "M-<backspace>" #'sp-backward-kill-sexp
           (:n :i) local "M-D" #'sp-kill-sexp
           (:i) local "<return>" #'sp-newline
           (:n :i) local "M-c" #'sp-clone-sexp
           (:n :i) local "M-C" #'sp-copy-sexp
           (:n :i) local "M-r" #'sp-raise-sexp
           (:n :i) local "M-e" #'sp-emit-sexp
           (:n :i) local "M-S" #'sp-split-sexp
           (:n :i) local "M-j" #'sp-join-sexp
           (:n :i) local "M-a" #'sp-absorb-sexp
           (:n :i) local "M-t" #'sp-transpose-sexp
           (:n :i) local "M-;" #'sp-comment
           (:n :i) local "M-I" #'sp-change-inner
           (:n :i) local "M-E" #'sp-change-enclosing
           (:n :i) local "C-M-N" #'sp-change-enclosing
           (:n :v :i) local "M-o" #'sp-beginning-of-sexp
           (:n :v :i) local "M-O" #'sp-end-of-sexp
           (:n :v :i) local "M-w" #'sp-beginning-of-next-sexp
           (:n :v :i) local "M-W" #'sp-beginning-of-previous-sexp
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
    ("e" sp-end-of-sexp "end of sexp")
    ("f" sp-forward-symbol "forward sym")
    ("b" sp-backward-symbol "backward sym")
    ("n" sp-forward-sexp "forward sexp")
    ("p" sp-backward-sexp "backward sexp")
    ("o" sp-up-sexp "up sexp")
    ("O" sp-backward-up-sexp "backward down sexp")
    ("I" sp-down-sexp "up sexp")
    ("i" sp-backward-down-sexp "backward down sexp")

    ("ci" sp-change-inner "change inner" :column "Actions")
    ("ce" sp-change-enclosing "change enclosing")
    ("xa" sp-absorb-sexp "absorb")
    ("xc" sp-convolute-sexp "convolute")
    ("xe" sp-emit-sexp "emit")
    ("xE" eval-last-sexp "eval")
    ("xj" sp-join-sexp "join")
    ("xr" sp-raise-sexp "raise")
    ("xs" sp-split-sexp "split")
    ("x/" sp-splice-sexp "splice")
    ("xb" sp-splice-sexp-killing-backward "splice killing backward")
    ("xf" sp-splice-sexp-killing-forward "splice killing forward")
    ("xt" sp-transpose-sexp "transpose")
    ("xn" sp-narrow-to-sexp "narrow to sexp")
    (";" sp-comment "comment")
    ("<tab>" indent-sexp "indent sexp")
    ("Eb" sp-extract-before-sexp "extract before")
    ("Ea" sp-extract-after-sexp "extract after")

    ("h" sp-slurp-hybrid-sexp "slurp hybrid" :exit t :column "Slurp/Barf/Yank/Kill")
    ("Sn" sp-add-to-next-sexp "add to next sexp")
    ("Sp" sp-add-to-previous-sexp "add to previous sexp")
    ("Sf" sp-forward-slurp-sexp "slurp forward")
    ("Sb" sp-backward-slurp-sexp "slurp backward")
    ("Bf" sp-forward-barf-sexp "barf forward")
    ("Bb" sp-backward-barf-sexp "barf backward")
    ("D" sp-kill-sexp "kill sexp")
    ("d" sp-backward-kill-sexp "backward kill sexp")
    ("cd" sp-clone-sexp "clone")
    ("cc" sp-copy-sexp "copy")
    ("cC" sp-backward-copy-sexp "copy backwards")
    ("u" undo "undo")

    ("?" (hydra-set-property 'hydra-sp :verbosity 1) "hide" :column "Etc")
    (":" execute-extended-command "EX")
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
           (local-set-key (kbd "C-l") #'hydra-sp/body)
           (p/load-lispy-parens-bindings)
           (setq-local evil-move-beyond-eol t))
       (:maps (:n :v :i) local "M-w" #'sp-slurp-hybrid-sexp)))))

;;;; load evil-smartparens
(use-package evil-smartparens
  :after (evil smartparens)
  :hook ((smartparens-mode smartparens-strict-mode)
         .
         evil-smartparens-mode))
