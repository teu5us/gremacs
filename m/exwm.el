;;; exwm.el --- EXWM configuration -*- lexical-binding: t; -*-

;;; exwm
(use-package exwm
  :commands (exwm-enable)
  :hook (exwm-mode . (lambda ()
                       (setq-local x-alt-keysym 'alt)))
  :init
  (setq mouse-autoselect-window t
        focus-follows-mouse 'auto-raise)
  (defun run-exwm ()
    "Launch exwm displaying battery and time."
    (interactive)
    (progn
      (exwm-enable)
      (display-battery-mode 1)
      (display-time-mode 1)))
  :config

;;;; boon
  (with-eval-after-load 'boon
    (push 'exwm-mode boon-special-mode-list))
;;;; function definitions
;;;;; key functions
  (defun make-screenshot ()
    (interactive)
    (start-process-shell-command "maim" nil "cd && screenshot"))
  (defun maimpick ()
    (interactive)
    (start-process-shell-command "maim" nil "cd && maimpick"))
  (defun emuwheelinertia ()
    (interactive)
    (message (format "Ball Scroll Inertia: %s"
                     (string-trim (shell-command-to-string "emuwheelinertia")))))
  (defun ballsens ()
    (interactive)
    (message (format "Ball Deceleration: %s"
                     (string-trim (shell-command-to-string "ballsens.sh")))))
  (defun ballscroll ()
    (interactive)
    (start-process-shell-command "ballscroll.sh" nil "ballscroll.sh"))
  (defun balllock-x ()
    (interactive)
    (start-process-shell-command "balllockdirection.sh x" nil "balllockdirection.sh x"))
  (defun balllock-y ()
    (interactive)
    (start-process-shell-command "balllockdirection.sh y" nil "balllockdirection.sh y"))
  (defun switch-system-im ()
    (interactive)
    (start-process-shell-command "xkb-switch" nil "xkb-switch -n"))
  (defun clipmenu ()
    (interactive)
    (start-process-shell-command "clipmenu" nil "clipmenu"))
  (defun print-volume ()
    (message "Volume: %s%%" (car (split-string (shell-command-to-string "pulsemixer --get-volume") "[ ]"))))
  (defun vol-up ()
    (interactive)
    (start-process-shell-command "pulsemixer" nil "pulsemixer --change-volume +5")
    (print-volume))
  (defun vol-down ()
    (interactive)
    (start-process-shell-command "pulsemixer" nil "pulsemixer --change-volume -5")
    (print-volume))
  (defun displayselect ()
    (interactive)
    (start-process-shell-command "displayselect" nil "displayselect"))
  (defun pavucontrol ()
    (interactive)
    (start-process-shell-command "pavucontrol" nil "pavucontrol"))
  ;; (defun recreate-vterm-popup ()
  ;;   (interactive)
  ;;   (+vterm/toggle t))
;;;; configuration
  (require 'exwm-config)
  (defun p/exwm-ws-number (n)
    (number-to-string (1+ n)))
  (setq exwm-workspace-index-map #'p/exwm-ws-number)
;;;;; global keys
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-x] . execute-extended-command)
          (\,@ (mapcar (lambda (i)
                         `(,(kbd (format "s-%d" (if (= i 10) 0 i))) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,(- i 1)))))
                       (number-sequence 1 10)))
          (\,@ (mapcar (lambda (i)
                         `(,(kbd (format "<s-kp-%d>" (if (= i 10) 0 i))) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,(- i 1)))))
                       (number-sequence 1 10)))
          (,(kbd "s-&") . (lambda (command)
                            (interactive (list (read-shell-command ">> ")))
                            (start-process-shell-command command nil command)))
          (,(kbd "s-b") . switch-to-buffer)
          (,(kbd "s-i") . exwm-input-toggle-keyboard)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-F") . exwm-floating-toggle-floating)
          (,(kbd "s-h") . evil-window-left)
          (,(kbd "s-l") . evil-window-right)
          (,(kbd "s-j") . evil-window-down)
          (,(kbd "s-k") . evil-window-up)
          (,(kbd "s-'") . aweshell-dedicated-toggle)
          (,(kbd "s-t") . vterm-popup)
          (,(kbd "M-s-t") . recreate-vterm-popup)
          (,(kbd "s-v") . counsel-set-clip)
          (,(kbd "s-a") . switch-system-im)
          (,(kbd "s-ф") . switch-system-im)
          (,(kbd "s-A") . pavucontrol)
          (,(kbd "s-c") . clipmenu)
          (,(kbd "s--") . vol-down)
          (,(kbd "s-=") . vol-up)
          (,(kbd "<XF86AudioLowerVolume>") . vol-down)
          (,(kbd "<XF86AudioRaiseVolume>") . vol-up)
          (,(kbd "s-p p") . make-screenshot)
          (,(kbd "s-p P") . maimpick)
          (,(kbd "s-q") . kill-buffer)
          (,(kbd "s-Q") . kill-buffer-and-window)
          (,(kbd "<s-f3>") . displayselect)
          (,(kbd "s-,") . google-translate-from-selection)
          (,(kbd "s-[") . emuwheelinertia)
          (,(kbd "s-u") . ballscroll)
          (,(kbd "s-e") . ballsens)))
;;;;; simulation keys
  (setq exwm-input-simulation-keys
        `((,(kbd "M-w") . [C-c])
          (,(kbd "C-.") . [C-c])
          (,(kbd "C-y") . [C-v])
          (,(kbd "C-s") . [C-f])
          (,(kbd "C-f") . [right])
          (,(kbd "C-b") . [left])
          (,(kbd "C-n") . [down])
          (,(kbd "C-p") . [up])
          (,(kbd "C-a") . [home])
          (,(kbd "C-e") . [end])
          (,(kbd "M-v") . [prior])
          (,(kbd "C-v") . [next])
          (,(kbd "C-d") . [delete])
          (,(kbd "C-k") . [S-end delete])
          (,(kbd "M-f") . [C-right])
          (,(kbd "M-b") . [C-left])))
;;;;; workspace-buffer switching
  (setq exwm-workspace-number 1)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
;;;;; exwm buffer naming
  (defun exwm-rename-buffer ()
    "Update buffer name with window name."
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))

  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
;;;;; exwm menu
  (easy-menu-define exwm-workspace-menu nil
    "Menu for Exwm Workspace.

Also used in `exwm-mode-line-workspace-map'."
    '("Exwm Workspace"
      ["Add workspace" exwm-workspace-add]
      ["Delete current workspace" exwm-workspace-delete]
      ["Move workspace to" exwm-workspace-move]
      ["Swap workspaces" exwm-workspace-swap]
      ["Move X window to" exwm-workspace-move-window]
      ["Move X window from" exwm-workspace-switch-to-buffer]
      ["Toggle minibuffer" exwm-workspace-toggle-minibuffer]
      ["Switch workspace" exwm-workspace-switch]
      ;; Place this entry at bottom to avoid selecting others by accident.
      ("Switch to" :filter
       (lambda (&rest _args)
         (mapcar (lambda (i)
                   `[,(format "workspace %d" i)
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch ,i))
                     (/= ,i exwm-workspace-current-index)])
                 (number-sequence 0 (1- (exwm-workspace--count))))))))

  (defvar exwm-mode-line-workspace-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] 'exwm-workspace-switch)
      (define-key map [mode-line mouse-3] exwm-workspace-menu)
      map)
    "Local keymap for EXWM mode line string.  See `exwm-mode-line-format'.")

  (defcustom exwm-mode-line-format
    `("["
      (:propertize (:eval (format "WS-%s" (p/exwm-ws-number exwm-workspace-current-index)))
                   local-map ,exwm-mode-line-workspace-map
                   face bold
                   mouse-face mode-line-highlight
                   help-echo "mouse-1: Switch to / add / delete to EXWM workspaces.
mouse-2: EXWM Workspace menu.
")
      "] ")
    "EXWM workspace in the mode line."
    :type 'sexp)


  ;; FIXME: Don't push the value.  Instead push a symbol.  If done, (1)
  ;; this will avoid duplicate entries for EXWM workspace (2) The mode
  ;; line string will change in sync with the value of
  ;; `exwm-mode-line-format'.
  (add-to-list 'mode-line-misc-info exwm-mode-line-format)
;;;; systray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
;;;; xim
  (require 'exwm-xim)
  (add-hook 'exwm-init-hook #'(lambda ()
                                (when (featurep 'evil)
                                  (evil-set-initial-state 'exwm-mode 'emacs))))
  (defvar s-space (aref [?\s-\ ] 0)
    "Key value for s-SPC.")
  (defvar m-space (aref [?\M-\ ] 0)
    "Key value for M-SPC.")
  (push ?\C-\\ exwm-input-prefix-keys)
  (push m-space exwm-input-prefix-keys)
  (push s-space exwm-input-prefix-keys)
  (exwm-xim-enable)
;;;; randr
  (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
  ;; (setq exwm-randr-workspace-monitor-plist '(0 "DP-2"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output eDP-1 --left-of HDMI-1 --auto")))
  (exwm-randr-enable))

;;; exwm-edit
(use-package exwm-edit
  :after exwm
  :commands (exwm-edit--compose))

;;; misc to fix later

  ;; (when (featurep! +sim-duplicate)
  ;;   (when (featurep! :personal russian)
  ;;     (add-to-list 'exwm-input-global-keys
  ;;                  `(,(kbd "s-ф") . switch-system-im))))


  ;; (set-popup-rule! "^\Pavucontrol" :slot -1 :size 0.4 :select t)

;;; exwm.el ends here
