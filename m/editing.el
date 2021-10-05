;;; editing.el --- Editing and navigation configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up evil-mode and supplementary plugins along with other
;; plugins to aid text editing and navigation in Emacs.

;;; Code:

;;;; keybinding macros
;; Shorter keybinding definitions
(defmacro p/bind (keyword state)
  (list 'defmacro keyword '(map key function)
     (list 'list ''with-eval-after-load '''evil
           (list 'list
                 ''evil-define-key
                 (list 'quote (list 'quote state))
                 (list 'list ''quote 'map)
                 '(kbd key)
                 'function))))

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
                  collect (list state map key function)))))

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
  :config
  (global-undo-tree-mode)
  :hook
  (prog-mode . (lambda ()
                 (:maps (:n :v :e) local "<leader>ou" #'undo-tree-visualize)))
  (text-mode . (lambda ()
                 (:maps (:n :v :e) local "<leader>ou" #'undo-tree-visualize))))

;;;; load evil
(use-package evil
  :after undo-tree
  :commands (evil-mode evil-set-leader)
  :hook
  (evil-mode . p/evil-modeline-im-setup)
  (after-init . (lambda ()
                  (evil-set-leader '(normal visual operator motion) (kbd p/evil-leader))
                  (evil-set-leader '(emacs replace insert) (kbd p/evil-emacs-leader))
                  (evil-set-leader '(normal visual operator motion) (kbd p/evil-localleader) t)
                  (evil-set-leader '(emacs replace insert) (kbd p/evil-emacs-localleader) t)
                  (evil-mode 1)))
;;;;; init
  :init
  (defcustom p/evil-leader "SPC"
    "Evil-mode leader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-emacs-leader "M-SPC"
    "Evil-mode emacs state leader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-localleader "<leader>m"
    "Evil-mode localleader key."
    :type 'string
    :group 'p/evil)
  (defcustom p/evil-emacs-localleader p/evil-localleader
    "Evil-mode emacs state localleader key."
    :type 'string
    :group 'p/evil)
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
	    evil-complete-all-buffers t)
;;;;; config
  :config
  (defun p/backward-delete-word ()
    (if (bound-and-true-p evil-mode)
	    (evil-delete-backward-word)
      (backward-kill-word))))

;;;; load evil-collection
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (setq evil-collection-setup-minibuffer t
	evil-collection-want-unimpaired-p t)
  :config
  (evil-collection-init))

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
  :after evil
  :config
  (:maps (:n :v) global "gc" #'evilnc-comment-operator))

;;;; load evil-surround
(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode 1))

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

;;;; do basic mapping
(:maps
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
   :a global "<leader>qK" #'save-buffers-kill-terminal
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
  :config
  (:maps (:n :v :e) global "<leader>sr" #'rg-menu))

(use-package iedit
  :diminish
  :bind (("C-;" . iedit-mode)
         :map consult-isearch-map
         ("C-;" . iedit-mode-from-isearch)))

(use-package wgrep)

;;;; evil additions for faster navigation
(use-package avy
  :bind
  ("M-g w" . avy-goto-word-0)
  ("M-g g" . avy-goto-line))

(use-package evil-easymotion
  :after evil
  :config
  (:map (:n :v) global "gs" evilem-map))

(use-package evil-snipe
  :diminish (evil-snipe-mode
             evil-snipe-local-mode
             evil-snipe-override-mode
             evil-snipe-override-local-mode)
  :after evil-easymotion
  :hook
  ((prog-mode text-mode) . evil-snipe-local-mode)
  ((prog-mode text-mode) . evil-snipe-override-local-mode)
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
(with-eval-after-load 'outshine
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
