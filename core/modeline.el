;;; modeline.el --- modeline configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module tweaks modeline the way I like.

;;; Code:

;;;; time and battery formats
(setq-default display-time-world-time-format "[%a %d %b %R %Z]"
              display-time-format "[%a %d %b %R]"
              battery-mode-line-format "[%b%p%%: %t]"
              display-time-day-and-date t
              display-time-24hr-format t
              display-time-default-load-average nil)

;;;; mode-line-mule-info
;; here we have no check if IM is active unlike in default configuration
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
;; force setting IM title to "SY" to reflect system input method
(defun p/cimt-system ()
  (setq current-input-method-title "SY")
  (force-mode-line-update))

(advice-add #'deactivate-input-method :after #'p/cimt-system)

;;;;; also in evil mode
;; also make evil mode update the IM in mode line

(defun p/evil-modeline-im-setup ()
  "Function to set up mode line input method indication in all evil states.
Hook this in `evil-mode-hook'."
  (defun set-im-tag ()
    (when evil-mode
      (setq current-input-method-title
            (or (cadddr (assoc-string evil-input-method input-method-alist))
                "SY"))
      (force-mode-line-update)))

  (setq evil-normal-state-tag " [N] "
        evil-insert-state-tag " [I] "
        evil-visual-state-tag " [V] "
        evil-motion-state-tag " [M] "
        evil-operator-state-tag " [O] "
        evil-replace-state-tag " [R] "
        evil-emacs-state-tag " [E] ")
  (advice-add #'toggle-input-method :after #'(lambda (&rest args)
                                               (set-im-tag)))
  (add-hook 'evil-normal-state-entry-hook #'set-im-tag)
  (add-hook 'evil-insert-state-entry-hook #'set-im-tag)
  (add-hook 'evil-emacs-state-entry-hook #'set-im-tag))

;;;; mode-line-position
;; remove line numbers from modeline if they are displayed on the side
(when global-display-line-numbers-mode
  (line-number-mode -1))

;;;; global-mode-string
;; display python virtual environment name
(setq-default global-mode-string
      '(""
        (pyvenv-virtual-env-name
           ("{" pyvenv-virtual-env-name "} "))
        display-time-string battery-mode-line-string))

;;;; mode-line-modes
;; I want modes to be displayed in brackets, not parens
(setq-default mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	  "["
	  `(:propertize ("" mode-name)
			help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			mouse-face mode-line-highlight
			local-map ,mode-line-major-mode-keymap)
	  '("" mode-line-process)
	  `(:propertize ("" minor-mode-alist)
			mouse-face mode-line-highlight
			help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
			local-map ,mode-line-minor-mode-keymap)
	  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		      'mouse-face 'mode-line-highlight
		      'local-map (make-mode-line-mouse-map
				  'mouse-2 #'mode-line-widen))
	  "]"
	  (propertize "%]" 'help-echo recursive-edit-help-echo)
	  " ")))

;;;; mode-line-format
;; update the whole mode line format
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
