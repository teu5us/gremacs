;;; completion.el --- Completion configuration -*- lexical-binding: t; -*-

;;;; marginalia
(use-package marginalia
  :hook (after-init . marginalia-mode))

;;;; orderless
(use-package orderless
  :after marginalia
  :custom
  (orderless-component-separator "[ -_&]")
  (orderless-matching-styles '(orderless-regexp
                               orderless-literal
                               orderless-initialism
                               orderless-flex))
  (completion-styles '(substring orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   `((command (styles orderless))
     (file (styles basic partial-completion orderless))
     (function (styles orderless))
     (variable (styles orderless))
     (buffer (styles orderless))
     (project-file (styles orderless))
     (unicode-name (styles orderless))
     (xref-location (styles orderless))
     (info-menu (styles orderless))))
  :config
  (with-eval-after-load 'org
    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps t)
    (advice-add #'org-olpath-completing-read :around
                (lambda (&rest args)
                  (minibuffer-with-setup-hook
                      (lambda () (setq-local completion-styles '(basic)))
                    (apply args))))))

;;;; vertico
(use-package vertico
  :disabled
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("M-RET" . minibufferforce-complete-and-exit)
              ("M-TAB" . minibuffer-complete)
              ("C-u" . universal-argument))
  :hook (after-init . vertico-mode)
  :config
  (:maps (:n) vertico-map "C-n" #'vertico-next
         (:n) vertico-map "C-p" #'vertico-previous)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (with-eval-after-load 'consult
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args))))

  ;; vertico-quick
  (p/require 'vertico "extensions/vertico-quick" 'vertico-quick)
  (define-key vertico-map (kbd "M-q") #'vertico-quick-insert)
  (define-key vertico-map (kbd "M-j") #'vertico-quick-jump)

  ;; vertico-directory
  (p/require 'vertico "extensions/vertico-directory" 'vertico-directory)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; vertico-flat
  (p/require 'vertico "extensions/vertico-flat" 'vertico-flat)

  ;; vertico-buffer
  (p/require 'vertico "extensions/vertico-buffer" 'vertico-buffer)

  ;; vertico-grid
  (p/require 'vertico "extensions/vertico-grid" 'vertico-grid)

  ;; vertico-unobtrusive
  (p/require 'vertico "extensions/vertico-unobtrusive" 'vertico-unobtrusive)

  ;; vertico-multiform
  (p/require 'vertico "extensions/vertico-multiform" 'vertico-multiform)

  (setq vertico-multiform-commands
        `((consult-imenu buffer)
          (consult-imenu-multi buffer)
          (execute-extended-command flat)
          (consult-outline buffer ,(lambda (_) (text-scale-set -1)))
          (switch-to-buffer unobtrusive)))

  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)))

  (define-key vertico-map (kbd "M-G") #'vertico-multiform-grid)
  (define-key vertico-map (kbd "M-F") #'vertico-multiform-flat)
  (define-key vertico-map (kbd "M-U") #'vertico-multiform-unobtrusive)

  (vertico-multiform-mode))

;;;; selectrum
(use-package selectrum
  :after orderless
  :custom
  (orderless-skip-highlighting #'(lambda () selectrum-is-active))
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-display-style '(vertical))
  :config
  (selectrum-mode +1))

;;;; consult
(use-package consult
  ;; :after (selectrum projectile)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Replace `goto-line' with `consult-goto-line'
  (advice-add #'goto-line :override #'consult-goto-line)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (defun p/setup-consult ()
    (setq consult-narrow-key "<")
    (with-eval-after-load 'projectile
      (setq consult-project-root-function #'projectile-project-root))
    (setq consult-preview-key nil)
    (advice-add 'recentf-open-files :override #'consult-recent-file)
    (global-set-key [remap imenu] 'consult-imenu)

    (defun consult-find-for-minibuffer ()
      "Search file with find, enter the result in the minibuffer."
      (interactive)
      (let* ((enable-recursive-minibuffers t)
             (default-directory (file-name-directory (minibuffer-contents)))
             (file (consult--find
                    (replace-regexp-in-string
                     "\\s-*[:([].*"
                     (format " (via find in %s): " default-directory)
                     (minibuffer-prompt))
                    #'consult--find-builder
                    (file-name-nondirectory (minibuffer-contents)))))
        (delete-minibuffer-contents)
        (insert (expand-file-name file default-directory))
        (exit-minibuffer)))

    (defun define-minibuffer-key (key &rest defs)
      "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
      (define-key minibuffer-local-map key
        (list 'menu-item nil defs :filter
              (lambda (d)
                (plist-get d (completion-metadata-get
                              (completion-metadata (minibuffer-contents)
                                                   minibuffer-completion-table
                                                   minibuffer-completion-predicate)
                              'category))))))

    (define-minibuffer-key "\C-s"
      'file #'consult-find-for-minibuffer)

    (:maps (:n :v) global "<leader>oi" #'imenu
           (:n :v) global "<leader>oI" #'consult-imenu-multi
           (:n :v :i :e) global "M-y" #'consult-yank-from-kill-ring))
  (add-hook 'after-init-hook #'p/setup-consult))

;;;;; lsp
(use-package consult-lsp
  ;; :after (consult lsp)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

;;;; embark
(use-package embark
  :commands (embark-act embark-dwim embark-bindings)
  :bind
  (("C-h B" . embark-bindings)
   ("C-." . embark-act)
   ("M-." . embark-dwim))
  :init
  (:maps (:n :v) global "C-." #'embark-act
         (:n :v) global "<leader>a" #'embark-act
         (:n :v) global "M-." #'embark-dwim)
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (kill-buffer which-key--buffer)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t))))
  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;;; embark-consult
(use-package embark-consult
  ;; :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; company
(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :bind
  (:map company-active-map
        ("C-f" . #'company-filter-candidates)
        ("C-k" . nil)
        ("C-j" . nil)
        ("<return>" . nil))
  (:map company-tng-map
        ("C-n" . #'company-select-next))
  :config
  (company-tng-configure-default)
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around #'just-one-face)
  (defun p/:company-backends (hook backends)
    (add-hook hook #'(lambda ()
		               (set (make-local-variable 'company-backends)
			                backends))))
  (p/:company-backends 'emacs-lisp-mode-hook
		               '((:separate company-capf
				                    company-files
				                    company-keywords
				                    company-yasnippet)))
  (:maps (:i) global "C-x C-f" #'company-files
         (:i) global "C-x C-o" #'company-capf
         (:i) global "C-x C-y" #'company-yasnippet)
  :hook
  (after-init . global-company-mode))
