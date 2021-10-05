;;; popup.el --- popup buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple functions to define popup windows.

;;; Code:

(require 'cl-lib)

;;;; vars
(defvar p/popup-buffers nil
  "A list containing popup buffer definitions.")

(defvar p/dba-backup nil
  "A backup for `display-buffer-alist'.")

;;;; functions
;;;;; pop up a buffer
(defun p/popup-active-p ()
  "Find the window with property \"p/popup\" in current frame."
  (get-window-with-predicate
   (lambda (win) (window-parameter win 'p/popup))
   nil nil nil))

(defun p/popup-buffer-window (buf)
  "Find the window displaying the target buffer BUF on all frames."
  (get-window-with-predicate
   (lambda (win)
     (and (window-parameter win 'p/popup)
          (eql buf (window-buffer win))))
   nil t nil))

(defun p/popup-buffer (buffer alist)
  "Pop up BUFFER using `display-buffer-in-side-window'.
Checks if any popup is active and if the target buffer is popped up in any
frame."
  (let ((active-popup (p/popup-active-p))
        (buffer-window (p/popup-buffer-window buffer)))
    (when active-popup
      (delete-window active-popup))
    (message "%s" (buffer-name (window-buffer active-popup)))
    (when buffer-window
      (delete-window buffer-window))
    (display-buffer-in-side-window buffer alist)))

;;;;; define popup rules
(cl-defun p/defpopup (key &key
                          (side 'bottom)
                          (height 20)
                          (width 60)
                          (mline 'none)
                          (hline 'none))
  "Define a popup window to be added to `display-buffer-alist'.

KEY must be a function or a regexp string.

SIDE must be 'top, 'bottom, 'left, or 'right.  Default: 'bottom.

HEIGHT is used if SIDE is 'top or 'bottom.  Default: 20.

WIDTH is used if SIDE is 'left or 'right.  Default: 60.

MLINE defines if mode line should be displayed.  Default: 'none.

HLINE defines if header line should be displayed.  Default: 'none."
  (when (not (memq (type-of key) '(function string)))
    (error "KEY must be a regexp string or a function"))
  (dolist (s (list side mline hline))
    (unless (eq (type-of s) 'symbol)
      (error "%s must be a symbol" s)))
  (dolist (s (list height width))
    (unless (or (null s)
                (memq (type-of s) '(integer float function)))
      (error "%s must be an integer, float, " s)))
  (add-to-list 'p/popup-buffers
               (list key '(p/popup-buffer)
                     (cons 'side side)
                     (cons 'window-height height)
                     (cons 'window-width width)
                     (cons 'dedicated t)
                     (list 'window-parameters
                           (cons 'p/popup t)
                           (cons 'mode-line-format mline)
                           (cons 'header-line-format hline)))))

;;;;; enable and restore default rules
(defun p/defpopups (specs)
  "Run `p/defpopup' for each element of SPECS."
  (dolist (spec specs)
    (apply #'p/defpopup spec)))

(defun p/restore-dba ()
  (interactive)
  (when p/dba-backup
    (setq display-buffer-alist p/dba-backup)))

(defun p/push-dba ()
  "Push all popup definitions in `p/popup-buffers' to `display-buffer-alist'."
  (interactive)
  (p/restore-dba)
  (setq p/dba-backup display-buffer-alist)
  (setq display-buffer-alist
        (append p/popup-buffers display-buffer-alist)))

;;;; define some rules
(p/defpopups `(("*helpful.*" :side right :width 0.5)
               ("*Customize.*" :side right :width 0.5)
               ("*Help.*" :side right :width 0.5)
               ("*Finder.*" :side right :width 0.5)
               ("Aweshell:.*" :side top :height 0.25)
               ("*Org Export Dispatcher" :side bottom)
               ("*rg*" :side right)
               ("*grep*" :side right)
               ("*Occur*" :side right)
               ("*Backtrace*" :side bottom :height 0.3)))

;;;; change quit-window to kill popups
(defun p/quit-window--popup-p (&optional kill window)
  "Filter `quit-window' args to kill popups."
  (list (when (window-parameter (selected-window) 'p/popup) t) window))

(advice-add #'quit-window :filter-args #'p/quit-window--popup-p)

;;;; hook everything after init
(add-hook 'after-init-hook #'p/push-dba)

;;; popup.el ends here
