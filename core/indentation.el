;; indentation.el --- Configure indentation -*- lexical-binding: t; -*-

(defun p/setup-indentation ()
  ;; (setq-default electric-indent-inhibit t)
  (setq-default backward-delete-char-untabify-method 'hungry)
  (setq-default indent-tabs-mode nil)
  (setq-default indent-line-function 'insert-tab)
  (setq-default tab-width 4)
  (setq-default c-basic-offset 4)
  (setq-default js-switch-indent-offset 4)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label 0)
  (c-set-offset (quote cpp-macro) 0 nil)
  ;; (defun smart-electric-indent-mode ()
  ;;   "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  ;;   (cond ((and (eq electric-indent-mode t)
  ;;               (member major-mode '(erc-mode text-mode)))
  ;;          (electric-indent-mode 0))
  ;;         ((eq electric-indent-mode nil) (electric-indent-mode 1))))
  ;; (add-hook 'post-command-hook #'smart-electric-indent-mode))
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

(add-hook 'after-init-hook #'p/setup-indentation)

(use-package hungry-delete
  :diminish hungry-delete-mode
  :custom
  (hungry-delete-join-reluctantly t)
  :hook (prog-mode . hungry-delete-mode))
