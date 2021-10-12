;;; pdf.el --- pdf-tools configuration -*- lexical-binding: t; -*-

;;;; pdf-tools
(use-package pdf-tools
  :commands (pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1)))
  :custom
  (pdf-view-continuous t)
  (pdf-view-display-size 'fit-height)
  :config
  (p/defpopup "*Outline.*" t :side 'right :width 0.3)
  (:maps (:n) pdf-view-mode-map "SPC" #'evil-send-leader
         (:n) pdf-view-mode-map "M-SPC" #'evil-send-localleader))

;;;; continuous scroll
(use-package pdf-continuous-scroll-mode
  :after pdf-tools
  :straight (:type git :host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :commands (pdf-continuous-next-page
             pdf-continuous-previous-page
             pdf-continuous-scroll-forward
             pdf-continuous-scroll-backward)
  :custom
  (pdf-continuous-step-size 1.25)
;;;;; config
  :config
  (defun p/pdf-scroll-forward (&optional arg)
    (interactive)
    (if (eq pdf-view-display-size 'fit-height)
        (evil-collection-pdf-view-next-line-or-next-page arg)
      (pdf-continuous-scroll-forward arg)))
  (defun p/pdf-scroll-backward (&optional arg)
    (interactive)
    (if (eq pdf-view-display-size 'fit-height)
        (evil-collection-pdf-view-previous-line-or-previous-page arg)
      (pdf-continuous-scroll-backward arg)))
;;;;;; override broken scrolling
  (defun p/pdf-continuous-scroll-forward-line (&optional arg)
  "Scroll upward by ARG lines if possible, else go to the next page.
This function is an adapted version of
`pdf-view-next-line-or-next-page'. Although the ARG is kept here,
this function generally works best without ARG is 1. To increase
the step size for scrolling use the ARG in
`pdf-continuous-scroll-forward'"
  (if pdf-continuous-scroll-mode
         (let ((current-file buffer-file-name)
               (hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
	         (print (format
                   "%s: window-total-height %s, frame-height %s\n
next line: vscroll value, second next line: output value (image-next-line)"
                   (alist-get 'pdf-scroll-window-status (window-parameters))
                   (window-total-height)
                   (frame-height))
                  (get-buffer-create "*pdf-scroll-log*"))
           (if (not (equal (alist-get 'pdf-scroll-window-status (window-parameters)) 'lower))
               (when (= (print
                         (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                         (get-buffer-create "*pdf-scroll-log*"))
                        (print (image-next-line arg) (get-buffer-create "*pdf-scroll-log*")))
	               (cond
	                ((not (window-full-height-p))
                   (condition-case nil
                       (window-resize (get-buffer-window) -1 nil t)
                     (error (delete-window)
                            (pop-to-buffer (get-file-buffer current-file))
                            (set-window-parameter nil 'pdf-scroll-window-status 'single)))
                   (image-next-line 1))
                  (t
                   (if (= (pdf-view-current-page) (pdf-cache-number-of-pages))
                       (message "No such page: %s" (+ (pdf-view-current-page) 1))
                     (display-buffer
                      (current-buffer)
                      `((display-buffer-below-selected)
                        (inhibit-same-window . t)
                        (window-height . ,window-min-height)))
                     (set-window-parameter nil 'pdf-scroll-window-status 'upper)
                     (windmove-down)
                     (set-window-parameter nil 'pdf-scroll-window-status 'lower)
                     (pdf-view-goto-page cur-page)
                     (pdf-view-next-page)
                     (when (/= cur-page (pdf-view-current-page))
                       (image-bob)
                       (image-bol 1))
                     (image-set-window-hscroll hscroll)
                     (windmove-up)
                     (image-next-line 1)))))
             (condition-case nil
                 (window-resize (get-buffer-window) +1 nil t)
               (error (windmove-up)
                      (delete-window)
                      (pop-to-buffer (get-file-buffer current-file))
                      (set-window-parameter nil 'pdf-scroll-window-status 'single)))
             (windmove-up)
             (image-next-line 1)
             (windmove-down)))
  (message "pdf-continuous-scroll-mode not activated")))
  (advice-add #'pdf-continuous-scroll-forward-line
              :override
              #'p/pdf-continuous-scroll-forward-line)
;;;;; hook
  :hook (pdf-view-mode . (lambda ()
                           (setq window-safe-min-height 1)
                           (setq-local mode-line-format 'nil)
                           ;; (setq-local mouse-autoselect-window nil)
                           (pdf-continuous-scroll-mode)
                           (:maps (:n) local "j" #'p/pdf-scroll-forward
                                  (:n) local "k"	#'p/pdf-scroll-backward
                                  (:n) local "J"	#'pdf-continuous-next-page
                                  (:n) local "K"	#'pdf-continuous-previous-page
                                  (:n) local "c"	#'pdf-continuous-scroll-mode))))


;;; pdf.el ends here
