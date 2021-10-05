;;; modeline.el --- modeline configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module tweaks modeline the way I like.

;;; Code:

;;; time and battery formats
(setq-default display-time-world-time-format "[%a %d %b %R %Z]"
              display-time-format "[%a %d %b %R]"
              battery-mode-line-format "[%b%p%%: %t]"
              display-time-day-and-date t
              display-time-24hr-format t
              display-time-default-load-average nil)

;;; mode-line-mule-info
(setq-default mode-line-mule-info
  `(""
    (:propertize ("" current-input-method-title)
		         help-echo (concat
			                ,(purecopy "Current input method: ")
			                current-input-method
			                ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
		         local-map ,mode-line-input-method-map
		         mouse-face mode-line-highlight
                 face custom-button)
    " "
    ,(propertize
      "%z"
      'help-echo 'mode-line-mule-info-help-echo
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)
    (:eval (mode-line-eol-desc))))

;;;; show system input method
(defun p/cimt-system ()
  (setq current-input-method-title "SY")
  (force-mode-line-update))

(defun p/cimt-nil (im)
  (setq current-input-method-title nil))

(advice-add #'activate-input-method :before #'p/cimt-nil)
(advice-add #'deactivate-input-method :after #'p/cimt-system)

;;;;; also in evil mode
(defun p/evil-modeline-im-setup ()
  (defun set-im-tag ()
    (when evil-mode
      (setq current-input-method-title
            (or (cadddr (assoc-string evil-input-method input-method-alist))
                "SY"))
      (force-mode-line-update)))

  (advice-add #'toggle-input-method :after #'(lambda (&rest args)
                                               (set-im-tag)))
  (add-hook 'evil-normal-state-entry-hook #'set-im-tag)
  (add-hook 'evil-insert-state-entry-hook #'set-im-tag)
  (add-hook 'evil-emacs-state-entry-hook #'set-im-tag))

;;; mode-line-position
;; remove line numbers
(when global-display-line-numbers-mode
  (line-number-mode -1))

;;; global-mode-string
(setq-default global-mode-string
      '(""
        (pyvenv-virtual-env-name
           ("{" pyvenv-virtual-env-name "} "))
        display-time-string battery-mode-line-string))

;;; mode-line-format
(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-position
                (vc-mode vc-mode)
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;;; modeline.el ends here
