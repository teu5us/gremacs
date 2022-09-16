;;; windows.el --- window management configuration -*- lexical-binding: t; -*-

;;;; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (defvar p/aw-keys "uhetonas"
    "String of keys to use in `ace-window'.")
  :custom
  (aw-ignore-current t)
  (aw-dispatch-always nil)
  (aw-scope 'frame)
  (aw-keys (string-to-list p/aw-keys)))

;;;; define popups
;; TODO: maybe switch to popper.el (https://github.com/karthink/popper)
(p/mod l popup)

;;;; splits
(defun p/split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable-p window)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-below)))
	(and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it vertically disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame nil 'nomini)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-height-threshold 0))
	   (when (window-splittable-p window)
	     (with-selected-window window
	       (split-window-below))))))))

(advice-add 'split-window-sensibly :override #'p/split-window-sensibly)

;;;; ediff splits
(setq
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-diff-options "-w"
 ediff-split-window-function 'split-window-horizontally)

;;;; window-purpose (disabled now)
(use-package window-purpose
  :disabled
  :after ace-window
  :diminish purpose-mode
  :commands (purpose-compile-user-configuration)
  :hook
  (after-init . purpose-mode)
  :custom
  (purpose-default-layout-file (expand-file-name ".purpose-layout" p/user-dir))
;;;;; init
  :init
  (require 'cl-lib)
  (defun p/define-purpose (&rest specs)
    (dolist (spec specs)
      (cl-destructuring-bind (rule-type &rest rule-values) spec
        (let ((rule-value (cons (car rule-values)
                                (cadr rule-values))))
          (cl-case rule-type
            ('mode (cl-pushnew rule-value purpose-user-mode-purposes))
            ('name (cl-pushnew rule-value purpose-user-name-purposes))
            ('regexp (cl-pushnew rule-value purpose-user-regexp-purposes))))))
    (purpose-compile-user-configuration))

;;;;;; popups
  (defvar purpose-x-popwin-major-modes nil)
  (defvar purpose-x-popwin-buffer-names nil)
  (defvar purpose-x-popwin-buffer-name-regexps nil)
  (cl-defun test-keys (&key mode name rx)
    (print mode)
    (print name)
    (print rx))
  (cl-defun p/define-popups (&key mode name rx)
    (cl-flet ((type-error (key type)
                          (error "Unsupported type `%s' for key `%s'"
                                 type key)))
      (when mode
        (cl-case (type-of mode)
          ('symbol (cl-pushnew mode purpose-x-popwin-major-modes))
          ('cons (if (listp mode)
                     (dolist (m mode)
                       (cl-pushnew m purpose-x-popwin-major-modes))
                   (type-error :mode (type-of mode))))
          (otherwise (type-error :mode (type-of mode)))))
      (when name
        (cl-case (type-of name)
          ('string (cl-pushnew name purpose-x-popwin-buffer-names
                               :test #'string-equal))
          ('cons (if (listp name)
                     (dolist (n name)
                       (cl-pushnew n purpose-x-popwin-buffer-names
                                   :test #'string-equal))
                   (type-error :name (type-of name))))
          (otherwise (type-error :name (type-of name)))))
      (when rx
        (cl-case (type-of rx)
          ('string (cl-pushnew rx purpose-x-popwin-buffer-name-regexps
                               :test #'string-equal))
          ('cons (if (listp rx)
                     (dolist (r rx)
                       (cl-pushnew r purpose-x-popwin-buffer-name-regexps
                                   :test #'string-equal))
                   (type-error :rx (type-of rx))))
          (otherwise (type-error :rx (type-of rx))))))
    (purpose-x-popwin-update-conf))
  :config
;;;;; load extensions
  (require 'window-purpose-x)
;;;;; load magit configuration
  (purpose-x-magit-multi-on)
;;;;; popwin emulation
  (setq purpose-x-popwin-position 'bottom
        purpose-x-popwin-height 15)
  (purpose-x-popwin-setup))

(with-eval-after-load 'window-purpose
  (p/define-popups :mode '(calendar-mode
                           occur-mode
                           help-mode)
                   :rx '("^*Finder-.*"
                         "^*helpful.*")))

;;; windows.el ends here
