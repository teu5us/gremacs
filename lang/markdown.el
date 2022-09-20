;;; markdown.el --- markdown configuration -*- lexical-binding: t; -*-

(use-package markdown-mode
  ;; :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-nested-imenu-heading-index nil)
  (markdown-fontify-code-blocks-natively t)
  :config
  (defun p/markdown-pandoc (beg end output-buffer)
    "Compiles markdown using pandoc, if available.  Returns its exit
code."
    (when (executable-find "pandoc")
      (call-process-region beg end "pandoc" nil output-buffer nil
                           "-f" "markdown"
                           "-t" "html"
                           "--mathjax"
                           "--highlight-style=pygments")))
  (setq markdown-command 'p/markdown-pandoc))

(use-package evil-markdown
  :diminish evil-markdown-mode
  :straight (:type git :host github :repo "Somelauw/evil-markdown")
  ;; :after (evil markdown-mode)
  :hook (markdown-mode . evil-markdown-mode))

(use-package markdown-toc
  :after markdown-mode)
