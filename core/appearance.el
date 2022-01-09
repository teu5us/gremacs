;;; appearance.el --- Core appearance configuration -*- lexical-binding: t; -*-

;;; Disable unnecessary interface
(menu-bar-mode -1)
(blink-cursor-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(bottom-divider-width . 1) default-frame-alist)
(push '(alpha . 100) default-frame-alist)

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

;;; Display time and battery
(display-time-mode 1)
(display-battery-mode 1)

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
  :hook (after-init . which-key-mode)
  :custom
  (which-key-paging-prefixes '("C-x"))
  (which-key-paging-key "C-h")
  ;; :bind (:map minibuffer-mode-map
  ;;             ("C-h n" . which-key-show-next-page-cycle)
  ;;             ("C-h p" . which-key-show-previous-page-cycle))
  :config
  (which-key-setup-minibuffer))

;;; Automatically visually fill text
(use-package visual-fill-column
  :commands (visual-fill-column-mode))

;;; customize mode line
(p/mod l modeline)

;;; Highlight TODO and stuff
(use-package hl-todo
  :diminish hl-todo-mode
  :hook (prog-mode . hl-todo-mode))
