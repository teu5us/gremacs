;;; appearance.el --- Core appearance configuration -*- lexical-binding: t; -*-

;;; Disable unnecessary interface
(defun p/setup-interface ()
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (unless (and (display-graphic-p) (eq system-type 'darwin))
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(bottom-divider-width . 1) default-frame-alist)
  (push '(alpha . 100) default-frame-alist)
  (push '(fullscreen . maximized) default-frame-alist))
(add-hook 'after-init-hook #'p/setup-interface)

;;; Hide the default buffer margins
(set-face-attribute 'fringe nil :background nil)

;;; Display column number
(column-number-mode 1)

;;; Inhibit unnecessary stuff
(setq
 ;; Don't disable Emacs startup screen:
 ;; we'll be using Dashboard
 inhibit-startup-screen nil
 initial-scratch-message nil ;; Disable scratch buffer message
 inhibit-startup-message t
 inhibit-startup-echo-area-message t)

;;; Line numbering
(line-number-mode -1)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
;; Calculate the width of line numbering panel beforehand
(defun p/calc-set-line-numbering-width ()
  (setq-local display-line-numbers-width
              (1+ (length (int-to-string (count-lines (point-min) (point-max)))))))
(add-hook 'prog-mode-hook #'p/calc-set-line-numbering-width)
(add-hook 'text-mode-hook #'p/calc-set-line-numbering-width)

;;; Smooth scrolling
;;;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;;;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;;; Simplify Yes/No Prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; ;;; Don't automatically switch to buffer visible in current frame
;; (setq switch-to-prev-buffer-skip nil)

;;; Use visual lines with word wrapping
(setq-default truncate-lines t
              word-wrap t
              word-wrap-by-category t
              fill-column 80)
(global-visual-line-mode)


;;; Set up a buffer predicate

;; I want to avoid auto switching to non-file buffers and buffers that are
;; already displayed in current frame.

(defvar p/buffer-predicate-names nil
  "Buffer names to be considered by buffer-predicate `p/file-buffer-p'.")
(defvar p/buffer-predicate-regexps nil
  "Buffer regexps to be considered by buffer-predicate `p/file-buffer-p'.")

(defun p/file-buffer-p (buf)
  "Filter buffers when automatically switching with `other-buffer'.
Chosen buffer must be a file buffer or a buffer stored in variable
`p/buffer-predicate-names'."
  (require 'dash)
  (when
      (and (not (-any-p #'(lambda (win)
                            (equal buf
                                   (window-buffer win)))
                        (window-list)))
           (or (member (buffer-name buf) p/buffer-predicate-names)
               (-any-p #'(lambda (rx) (string-match-p rx (buffer-name buf)))
                       p/buffer-predicate-regexps)
               (buffer-file-name buf)))
    t))
(push "*scratch*" p/buffer-predicate-names)
(push '(buffer-predicate . p/file-buffer-p) default-frame-alist)

;;; Hide modes from mode-line
(use-package diminish
  :commands (diminish)
  :config
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode))

;;; Display key hints
(use-package which-key
  :diminish which-key-mode
  ;; :hook (after-init . which-key-mode)
  :commands (which-key-mode)
  :init
  (add-hook 'after-init-hook #'which-key-mode -90)
  :custom
  (which-key-paging-prefixes '("C-x"))
  (which-key-paging-key "C-h")
  (which-key-allow-evil-operators t)
  ;; :bind (:map minibuffer-mode-map
  ;;             ("C-h n" . which-key-show-next-page-cycle)
  ;;             ("C-h p" . which-key-show-previous-page-cycle))
  :config
  (which-key-setup-minibuffer))

;;; Automatically visually fill text
(use-package visual-fill-column
  :commands (visual-fill-column-mode))

;;; Customize window management
(setq display-buffer-base-action
      '(display-buffer--maybe-same-window
        display-buffer-reuse-window
        display-buffer-in-previous-window
        display-buffer-below-selected
        display-buffer-at-bottom
        display-buffer-pop-up-frame))

;;; Whitespace mode style
(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        space-before-tab
        newline
        indentation
        empty
        space-after-tab
        space-mark
        tab-mark
        newline-mark))

;;; customize mode line
(p/mod l modeline)

;;; load fonts
(defcustom p/main-font "Fira Code"
  "Font to use."
  :type 'string
  :group 'p/font)

(defcustom p/main-font-size 10
  "Font size."
  :type 'integer
  :group 'p/font)

(defcustom p/variable-pitch-font "Arimo"
  "Variable pitch font to use."
  :type 'string
  :group 'p/font)

(defcustom p/variable-pitch-font-size 10
  "Font size."
  :type 'integer
  :group 'p/font)

(defun p/fonts ()
  `((default ,p/main-font ,p/main-font-size)
    (variable-pitch ,p/variable-pitch-font ,p/variable-pitch-font-size)))

(defun p/load-font (face font size &optional frame)
  (with-selected-frame frame
    (when (member font (font-family-list))
      (set-face-attribute face nil
                          :font (format "%s-%d" font size)))))

(defun p/load-fonts (&optional frame)
  (let ((-frame (or frame (selected-frame))))
    (dolist (spec (p/fonts))
      (apply #'p/load-font (append spec (list -frame))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'p/load-fonts)
  (add-hook 'after-init-hook #'p/load-fonts))

;;; Highlight TODO and stuff
(use-package hl-todo
  :diminish hl-todo-mode
  :hook (prog-mode . hl-todo-mode))

;;; volatile highlights
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))
