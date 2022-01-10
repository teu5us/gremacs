;;; completion.el --- Completion configuration -*- lexical-binding: t; -*-

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :after marginalia
  :custom
  (orderless-matching-styles '(orderless-regexp
                               orderless-literal
                               orderless-initialism
                               orderless-flex))
  (completion-category-overrides
   `((command (styles orderless))
     (file (styles partial-completion orderless))
     (function (styles orderless))
     (variable (styles orderless))
     (buffer (styles orderless))
     (project-file (styles orderless))
     (unicode-name (styles orderless))
     (xref-location (styles orderless))
     (info-menu (styles orderless)))))

(use-package selectrum
  :after orderless
  :custom
  (orderless-skip-highlighting #'(lambda () selectrum-is-active))
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-display-style '(vertical))
  :config
  (selectrum-mode +1))

(use-package consult
  :after (selectrum projectile)
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
  (setq consult-narrow-key "<")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-preview-key nil)
  (advice-add 'recentf-open-files :override #'consult-recent-file)
  (advice-add 'imenu :override #'consult-imenu)

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
         (:n :v) global "<leader>oI" #'consult-imenu-multi))

(use-package consult-lsp
  :after (consult lsp)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

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

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :bind (:map company-active-map
	          ("C-f" . #'company-filter-candidates)
              ("C-k" . nil)
              ("C-j" . nil)
              ("<return>" . nil))
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
