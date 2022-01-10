;;; editing.el --- Editing and navigation configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up evil-mode and supplementary plugins along with other
;; plugins to aid text editing and navigation in Emacs.

;;; Code:

;;;; keybinding macros
;; Shorter keybinding definitions
(defmacro p/bind (keyword state)
  (let ((evil-call `(with-eval-after-load 'evil
                      (evil-define-key ',state
                        ,(quote ,(if (or (eq map 'global)
                                         (eq map 'local))
                                     (backquote ',map)
                                   map))
                        ,(quote (kbd ,key))
                        ,(quote ,function)))))
    `(defmacro ,keyword (map key function)
       (backquote ,evil-call))))

(p/bind :n normal)
;; ==>>
;; (defmacro :n (map key function)
;;   `(with-eval-after-load 'evil
;;      (evil-define-key 'normal ',map (kbd ,key) ,function)))

(p/bind :v visual)
(p/bind :e emacs)
(p/bind :i insert)
(p/bind :o operator)
(p/bind :r replace)
(p/bind :m motion)

(defmacro :map (states map key function)
  (require 'cl-lib)
  (let ((-states (if (and (symbolp states)
                          (eq states :a))
                     '(:n :v :e :i :o :r :m)
                   states)))
    `(progn
       ,@(cl-loop for state in -states
                  collect (list state
                                map
                                (if (symbolp key)
                                        (symbol-value key)
                                      key)
                                function)))))

(defmacro :maps (&rest decls)
  (require 'cl-lib)
  `(progn
     ,@(cl-loop for (states map key function) on decls by #'cddddr
                collect `(:map ,states ,map ,key ,function))))


;;;; load undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :demand t
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" p/user-dir))))
  :config
  (global-undo-tree-mode)
  :hook
  (prog-mode . (lambda ()
                 (:maps (:n :v :e) local "<leader>ou" #'undo-tree-visualize)))
  (text-mode . (lambda ()
                 (:maps (:n :v :e) local "<leader>ou" #'undo-tree-visualize))))

;;;; load boon
(use-package boon
  :demand t
  :hook
  (evil-emacs-state-entry . turn-on-boon-mode)
  (evil-emacs-state-exit . turn-off-boon-mode)
  :functions (boon-set-insert-like-state
              boon-set-command-state)
  :bind (:map boon-command-map
              ("<backspace>" . backward-delete-char)
              ("C-." . embark-act)
              ("C-\\" . p/boon-toggle-im))
  :custom
  (boon-command-cursor-type 'box)
  (boon-input-cursor-type 'bar)
  :init
  (defun p/check-for-boon (f &rest r)
    (if (not (or boon-mode boon-local-mode))
        (apply f r)
      (activate-input-method boon-input-method)
      (prog1
          (apply f r)
        (deactivate-input-method))))
  (advice-add #'read-char :around #'p/check-for-boon)

  (defun p/boon-toggle-im ()
    (interactive)
    (boon-set-insert-like-state)
    (toggle-input-method)
    (boon-set-command-state))
  :config
  (defun p/boon-modeline-string ()
    (concat " <" (boon-state-string) ">"))
  (advice-add #'boon-modeline-string :override #'p/boon-modeline-string)
  (p/require 'boon 'boon-qwerty)
  (p/boon-modeline-im-setup))

;;;; load multiple-cursors
(use-package multiple-cursors
  :bind (:map boon-command-map
              ("m" . p/mc-map))
  :commands (mc/edit-lines
             mc/mark-all-like-this
             mc/mark-next-like-this
             mc/skip-to-next-like-this
             mc/unmark-next-like-this
             mc/mark-previous-like-this
             mc/skip-to-previous-like-this
             mc/unmark-previous-like-this
             mc/vertical-align
             mc/mark-all-in-region-regexp
             mc/insert-numbers
             mc/insert-letters)
  :custom
  (mc/always-run-for-all t)
  (mc/always-repeat-command t)
  :init
  (define-prefix-command 'p/mc-map)
  (:maps (:n :v) global "M-m" 'p/mc-map)
  (dolist (mapping '(("l" . mc/edit-lines)
                     ("a" . mc/mark-all-like-this)
                     ("n" . mc/mark-next-like-this)
                     ("N" . mc/skip-to-next-like-this)
                     ("M-n" . mc/unmark-next-like-this)
                     ("p" . mc/mark-previous-like-this)
                     ("P" . mc/skip-to-previous-like-this)
                     ("M-p" . mc/unmark-previous-like-this)
                     ("|" . mc/vertical-align)
                     ("s" . mc/mark-all-in-region-regexp)
                     ("0" . mc/insert-numbers)
                     ("A" . mc/insert-letters)))
    (bind-key (kbd (car mapping)) (cdr mapping) p/mc-map)))

;;;; load evil
(use-package evil
  :after undo-tree
  :commands (evil-mode evil-set-leader)
  :hook
  (evil-mode . p/evil-modeline-im-setup)
  (after-init . (lambda ()
                  (:maps (:n :v :o :m) global p/evil-leader 'leader-map
                         (:n :v :o :m) global p/evil-localleader 'localleader-map
                         (:e :r :i) global p/evil-emacs-leader 'leader-map
                         (:e :r :i) global p/evil-emacs-localleader 'localleader-map)
                  (evil-mode 1)))
;;;;; init
  :init
  ;; define and load leaders
  (define-prefix-command 'leader-map)
  (define-prefix-command 'localleader-map)
  (global-set-key (kbd "<leader>") 'leader-map)
  (global-set-key (kbd "<localleader>") 'localleader-map)
  (defun p/set-mode-local-leaders ()
    (local-set-key (kbd "<leader>") 'leader-map)
    (local-set-key (kbd "<localleader>") 'localleader-map))
  (add-hook 'text-mode-hook #'p/set-mode-local-leaders -90)
  (add-hook 'prog-mode-hook #'p/set-mode-local-leaders -90)
  (defcustom p/evil-leader "SPC"
    "Evil-mode leader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-emacs-leader "M-SPC"
    "Evil-mode emacs state leader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-localleader "SPC m"
    "Evil-mode localleader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-emacs-localleader "M-SPC m"
    "Evil-mode emacs state localleader key."
    :type 'string
    :group 'p/evil)

  ;; set some stuff before we load evil
  (setq evil-want-integration t
	    evil-want-keybinding nil
	    evil-want-C-u-scroll t
	    evil-want-C-u-scroll t
	    evil-want-Y-yank-to-eol t
	    evil-search-module 'evil-search
	    evil-split-window-below t
	    evil-vsplit-window-right t
	    evil-undo-system 'undo-tree
	    evil-mode-line-format '(before . mode-line-mule-info)
	    evil-echo-state nil
	    evil-kill-on-visual-paste t
	    evil-complete-all-buffers t
        evil-cross-lines t
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t)
;;;;; config
  :config
  (defun p/backward-delete-word ()
    (if (bound-and-true-p evil-mode)
	    (evil-delete-backward-word)
      (backward-kill-word)))
  (defun p/goto--line (count)
    (let* ((col (current-column)))
      (goto-char (point-min))
      (forward-line (1- count))
      (goto-char (let ((tarcol (+ (line-beginning-position)
                                  col)))
                   (if (> tarcol (line-end-position))
                       (line-end-position)
                     tarcol)))))
  (evil-define-motion p/evil-goto-line (count)
    "Go to line COUNT preserving column if possible.
By default the last line, but not the end of buffer."
    :jump t
    :type line
    (if (null count)
        (with-no-warnings
          (p/goto--line (count-lines (point-min)
                                     (point-max))))
      (p/goto--line count)))
  (advice-add #'evil-goto-line :override #'p/evil-goto-line)
  (advice-add #'evil-force-normal-state :after #'evil-ex-nohighlight)
  )

;;;; load evil-collection
(use-package evil-collection
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-want-unimpaired-p t)
  :hook
  (evil-mode . evil-collection-init)
  (evil-collection-unimpaired-mode . (lambda ()
                                       (diminish 'evil-collection-unimpaired-mode))))

;;;; load evil-goggles to visualize evil commands
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :hook (after-init . evil-goggles-mode)
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-duration 0.1)
  (evil-goggles-use-diff-faces))

;;;; load evil-nerd-commenter
(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator)
  :init
  (:maps (:n :v) global "gc" #'evilnc-comment-operator))

;;;; load evil-surround
(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

;;;;; complement with embrace
(use-package embrace
  ;; :after expand-region
  :bind ("C-," . embrace-commander)
  :config
  (:maps (:n :v :e) global "C-," #'embrace-commander)
  (defun p/embrace--hide-help-buffer ()
    (when (buffer-live-p embrace--help-buffer)
      (let ((win (get-buffer-window embrace--help-buffer)))
        ;; Set `quit-restore' window parameter to fix evil-embrace/#5
        (set-window-parameter
         win 'quit-restore
         (list 'window 'window (selected-window) embrace--help-buffer))
        (deactivate-mark)
        (with-selected-window (select-window win)
          (quit-window)))))
  (advice-add #'embrace--hide-help-buffer :override #'p/embrace--hide-help-buffer)
  :hook (org-mode . embrace-org-mode-hook))

(use-package evil-embrace
  ;; :after (embrace evil-surround)
  :hook
  (evil-surround-mode . evil-embrace-enable-evil-surround-integration))

;;;; load evil-numbers
(use-package evil-numbers
  :after evil
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt
             evil-numbers/inc-at-pt-incremental
             evil-numbers/dec-at-pt-incremental)
  :init
  (:maps (:n :v) global "+" #'evil-numbers/inc-at-pt
         (:n :v) global "-" #'evil-numbers/dec-at-pt
         (:n :v) global "g +" #'evil-numbers/inc-at-pt-incremental
         (:n :v) global "g -" #'evil-numbers/dec-at-pt-incremental))

;;;; load evil-traces
(use-package evil-traces
  :diminish
  :after evil
  :hook
  (after-init . evil-traces-use-diff-traces)
  (after-init . evil-traces-mode))

;;;; load anzu
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

;;;; do basic mapping
(:maps
   :a global "<leader>" 'leader-map
   ;; :a global "<localleader>" 'localleader-map
   (:n) global "C-v" #'evil-visual-block
   :a global "<leader>fs" #'save-buffer
   :a global "<leader>." #'find-file
   :a global "<leader>ff" #'find-file
   :a global "<leader>fp" #'find-file-user-dir
   :a global "<leader>fP" #'find-file-conf-dir
   :a global "<leader>fr" #'recentf-open-files
   :a global "<leader>," #'switch-to-buffer
   :a global "<leader>bb" #'switch-to-buffer
   :a global "<leader>bd" #'kill-current-buffer
   :a global "<leader>bD" #'kill-buffer-and-window
   :a global "<leader>qK" #'save-buffers-kill-emacs
   :a global "<leader>qq" #'save-buffers-kill-terminal
   :a global "<leader>br" #'revert-buffer-noconfirm
   :a global "<leader>;" #'eval-expression
   :a global "<leader>u" #'universal-argument
   :a global  "<leader>fu" #'sudo-edit
   :a global "M-y" #'yank-from-kill-ring
   :a global "C-s" #'consult-isearch
   )

;;;; search/replace stuff
(use-package rg
  :after evil
  :defer 1
  :config
  (:maps (:n :v :e) global "<leader>sr" #'rg-menu))

(use-package iedit
  :diminish
  :bind (("C-;" . iedit-mode)
         :map consult-isearch-map
         ("C-;" . iedit-mode-from-isearch)))

(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode))

;;;; evil additions for faster navigation
(use-package avy
  :commands
  (avy-goto-char
   avy-goto-word-1
   avy-goto-word-0
   avy-goto-line
   avy-goto-end-of-line
   avy-move-line
   avy-move-region
   avy-kill-whole-line
   avy-kill-region
   avy-copy-line
   avy-copy-region)
  :init
  (global-set-key (kbd "C-x g")
                  (defhydra hydra-avy (:exit t :hint nil)
                    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] char        ^^
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
                    ("c" avy-goto-char)
                    ("w" avy-goto-word-1)
                    ("W" avy-goto-word-0)
                    ("l" avy-goto-line)
                    ("L" avy-goto-end-of-line)
                    ("m" avy-move-line)
                    ("M" avy-move-region)
                    ("k" avy-kill-whole-line)
                    ("K" avy-kill-region)
                    ("y" avy-copy-line)
                    ("Y" avy-copy-region))))

(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-quickscope
  :after evil
  :custom
  (evil-quickscope-cross-lines t)
  (evil-quickscope-accepted-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ")
  :hook ((text-mode prog-mode) . turn-on-evil-quickscope-always-mode))

(use-package evil-easymotion
  :after evil
  :commands (evilem-map evilem-create)
  :config
  (:map (:n :v) global "gs" evilem-map))

(use-package evil-snipe
  :diminish (evil-snipe-mode
             evil-snipe-local-mode
             evil-snipe-override-mode
             evil-snipe-override-local-mode)
  :after evil
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-keys nil)
  :hook
  ((prog-mode text-mode org-mode) . evil-snipe-local-mode)
  ((prog-mode text-mode org-mode) . evil-snipe-override-local-mode)
  :config
  (define-key evil-snipe-parent-transient-map (kbd "C-;")
    (evilem-create 'evil-snipe-repeat
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))

;;;;; support sniping with active input-method
(defvar khaoos-input-method-last-raw-key nil
  "The last key pressed with an input method switched on but ignoring conversion
of the input method.")

(defun khaoos-capture-input-mode-raw-key (key)
  "Function captures an input key ignoring the current input method.
Doesn't work for complex input methods which use event loops."
  (setq khaoos-input-method-last-raw-key (char-to-string key)))

(defun khaoos-activate-input-method (input-method)
  "Defines an advise for a function which implements current input method."
  ;; We don't bother ourselves to remove the advise when we deactivate the input method.
  ;; The chances are high that we'll reuse it.
  ;; We may end up with several advices for different input methods if an user uses them.
  ;; It doesn't matter as the only one work at the moment.
  ;; I saw a case when input-method-function was equal to 'list'! So there is addition check
  ;; on current-input-method
  (if (and current-input-method input-method-function)
      (advice-add input-method-function :before #'khaoos-capture-input-mode-raw-key)))

(advice-add 'activate-input-method :after #'khaoos-activate-input-method)

(defcustom khaoos-evil-escape-ignore-input-method nil
  "If non-nil then the key sequence can be entered ignoring the current input method if any."
  :type 'boolean
  :group 'evil-escape)

(defun khaoos-evil-escape-p ()
  "Return non-nil if evil-escape can run.
Edited by khaoos to implement the ability of ignoring the input method"
  (and evil-escape-key-sequence
       (not evil-escape-inhibit)
       (or (window-minibuffer-p)
           (bound-and-true-p isearch-mode)
           (memq major-mode '(ibuffer-mode
                              image-mode))
           (evil-escape--is-magit-buffer)
           (and (fboundp 'helm-alive-p) (helm-alive-p))
           (or (not (eq 'normal evil-state))
               (not (eq 'evil-force-normal-state
                        (lookup-key evil-normal-state-map [escape])))))
       (not (memq major-mode evil-escape-excluded-major-modes))
       (not (memq evil-state evil-escape-excluded-states))
       (or (not evil-escape-enable-only-for-major-modes)
           (memq major-mode evil-escape-enable-only-for-major-modes))
       (or (equal (this-command-keys) (evil-escape--first-key))
           (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal khaoos-input-method-last-raw-key (evil-escape--first-key))) ;;khaoos+
           (and evil-escape-unordered-key-sequence
                (or (equal (this-command-keys) (evil-escape--second-key))))
           (and evil-escape-unordered-key-sequence ;;khaoos+
                khaoos-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal khaoos-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
       (not (cl-reduce (lambda (x y) (or x y))
                       (mapcar 'funcall evil-escape-inhibit-functions)
                       :initial-value nil))))

(defun khaoos-evil-escape-pre-command-hook ()
  "evil-escape pre-command hook.
Edited by khaoos to implement the ability of ignoring the input method"
  (with-demoted-errors "evil-escape: Error %S"
    (when (khaoos-evil-escape-p)
      (let* ((modified (buffer-modified-p))
             (inserted (evil-escape--insert))
             (fkey (elt evil-escape-key-sequence 0))
             (skey (elt evil-escape-key-sequence 1))
             (evt (read-event nil nil evil-escape-delay)))
        (when inserted (evil-escape--delete))
        (set-buffer-modified-p modified)
        (cond
         ((and (characterp evt)
               (or (and (or (equal (this-command-keys) (evil-escape--first-key)) ;;khaoos*
                            (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                                 current-input-method ;;khaoos+
                                 (equal khaoos-input-method-last-raw-key (evil-escape--first-key)))) ;;khaoos+
                        (char-equal evt skey))
                   (and evil-escape-unordered-key-sequence
                        (or (equal (this-command-keys) (evil-escape--second-key)) ;;khaoos*
                            (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                                 current-input-method ;;khaoos+
                                 (equal khaoos-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
                        (char-equal evt fkey))))
          (evil-repeat-stop)
          (when (evil-escape-func) (setq this-command (evil-escape-func))))
         ((null evt))
         (t (setq unread-command-events
                  (append unread-command-events (list evt)))))))))

(advice-add 'evil-escape-pre-command-hook :override #'khaoos-evil-escape-pre-command-hook)

(defun khaoos-evil-read-key-respect-input-method (evil-read-key-result)
  "Gets the result of evil-read-key function and converts it according the current input method
which at the moment could be a method of a family of quail input methods"
  (if (and (characterp evil-read-key-result)
           current-input-method
           (equal input-method-function 'quail-input-method))
      (let* ((translated-key-list (quail-lookup-key (char-to-string evil-read-key-result)))
             (translated-key (if (equal (length translated-key-list) 1)
                                 (car translated-key-list)
                               evil-read-key-result)))
        translated-key)
    evil-read-key-result))

(advice-add 'evil-read-key :filter-return 'khaoos-evil-read-key-respect-input-method)

(defun khaoos-run-evil-command-respect-input-method (evil-command)
  "Runs interactively evil command evil-command which now respects the current input method"
  ;; if we are in the mode which prohibits input method we do a trick
  (if (and evil-input-method (not current-input-method))
      (evil-without-input-method-hooks
       (activate-input-method evil-input-method)
       (condition-case err
           (call-interactively evil-command)
         (error
          (inactivate-input-method)
          (signal (car err) (cdr err))))
       (inactivate-input-method))
    (call-interactively evil-command)))

(with-eval-after-load 'evil-macros
  (evil-define-operator khaoos-evil-replace ()
    "Wrapper of evil-replace to make it respect input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-replace))

  (evil-define-motion khaoos-evil-find-char ()
    "Wrapper of evil-find-char to make it respect input method"
    :type exclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-f))

  (evil-define-motion khaoos-evil-find-char-to ()
    "Wrapper of evil-find-char-to to make it respect input method"
    :type exclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-t))

  (evil-define-motion khaoos-evil-find-char-backward ()
    "Wrapper of evil-find-char-backward to make it respect input method"
    :type exclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-F))

  (evil-define-motion khaoos-evil-find-char-to-backward ()
    :type exclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-T))

  (evil-define-motion khaoos-evil-snipe-s ()
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-s))

  (evil-define-motion khaoos-evil-snipe-S ()
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-S))

  (evil-define-motion khaoos-evil-snipe-x ()
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-x))

  (evil-define-motion khaoos-evil-snipe-X ()
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-snipe-X))

  (evil-define-operator khaoos--insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (let ((a (read-char "Input a character to insert:" t)))
      (insert-char a)))

  (evil-define-operator khaoos-insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'khaoos--insert-one-char))

  (evil-define-operator khaoos--append-one-char ()
    "Switches to insert mode just to append one character"
    (interactive)
    (let ((a (read-char "Input a character to append:" t)))
      (unless (eolp) (forward-char))
      (insert-char a)
      (unless (eolp) (backward-char))))

  (evil-define-operator khaoos-append-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'khaoos--append-one-char)))


(with-eval-after-load 'evil-integration
  (defun khaoos-avy-goto-char ()
    "Make `evil-avy-go-to-char' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-char))

  (evil-define-avy-motion khaoos-avy-goto-char inclusive)

  (defun khaoos-avy-goto-char-2 ()
    "Make `evil-avy-go-to-char-2' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-char-2))

  (evil-define-avy-motion khaoos-avy-goto-char-2 inclusive)

  (defun khaoos-avy-goto-word-or-subword-1 ()
    "Make `evil-avy-go-to-char' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-word-or-subword-1))

  (evil-define-avy-motion khaoos-avy-goto-word-or-subword-1 exclusive))

;;;;;; map redefined evil commands
(:maps
  (:n) global "r" #'khaoos-evil-replace
  (:n :v :o) global "<leader>sc" #'evil-khaoos-avy-goto-char
  (:n :v :o) global "<leader>sC" #'evil-khaoos-avy-goto-char-2
  (:n :v :o) global "<leader>sw" #'evil-khaoos-avy-goto-word-or-subword-1
  (:n :v :o) evil-snipe-local-mode-map "f" #'khaoos-evil-find-char
  (:n :v :o) evil-snipe-local-mode-map "F" #'khaoos-evil-find-char-backward
  (:n :v :o) evil-snipe-local-mode-map "t" #'khaoos-evil-find-char-to
  (:n :v :o) evil-snipe-local-mode-map "T" #'khaoos-evil-find-char-to-backward
  (:n) evil-snipe-local-mode-map "s" #'khaoos-evil-snipe-s
  (:v :o) evil-snipe-local-mode-map "z" #'khaoos-evil-snipe-s
  (:n) evil-snipe-local-mode-map "S" #'khaoos-evil-snipe-S
  (:v :o) evil-snipe-local-mode-map "Z" #'khaoos-evil-snipe-S)

;;;; create on outshine leader binding
(with-eval-after-load
    'outshine
  (:map (:n :v :e) global "<leader>l" outline-mode-prefix-map))

;;;; macrostep
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-mode)
              ("C-c C-e" . pp-macroexpand-last-sexp)))

;;;; expand-region
(use-package expand-region
  :commands (er/expand-region)
  :hook ((text-mode prog-mode) . (lambda ()
                                   (local-set-key (kbd "C-'") #'er/expand-region))))

;;; editing.el ends here
