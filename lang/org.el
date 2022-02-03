;;; org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file configures org-mode and various additions to it.

;;; Code:

;;;; org mode itself
(use-package org
  :mode ("\\.org\\'" . org-mode)
;;;;; custom
  :custom
;;;;;; headings
  (org-num-skip-footnotes t)
  (org-num-skip-commented t)
  (org-cycle-separator-lines 0)
  (org-hide-leading-stars t)
;;;;;; todo
  (org-enforce-todo-dependencies t)
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                       (sequence "CANCELED(c)")))
  ;; nav
  (org-special-ctrl-a/e t)
;;;;;; code
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
;;;;; config
  :config
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
;;;;; hooks
  :hook
  (org-mode . (lambda ()
                (diminish 'org-num-mode)
                (diminish 'org-indent-mode))))

;;;; evil-org
(use-package evil-org
  ;; :after (evil org)
  :commands (evil-org-define-eol-command)
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :init
;;;;; keys
  (add-hook 'org-mode-hook
            #'(lambda ()
                (:maps (:n :v :m :o) local "$" #'evil-end-of-line ;
                       (:n :v :m :o) local "0" #'evil-digit-argument-or-evil-beginning-of-line
                       (:n :v) local "<localleader>-" #'org-ctrl-c-minus
                       (:n :v) local "<localleader>*" #'org-ctrl-c-star
                       (:n :v) local "<localleader>/" #'org-sparse-tree
                       (:n :v) local "C-S-h" #'org-shiftleft
                       (:n :v) local "C-S-l" #'org-shiftright
                       (:n :v) local "<localleader>iH" (evil-org-define-eol-command
                                                        org-insert-heading)
                       (:n :v) local "<localleader>ih" (evil-org-define-eol-command
                                                        org-insert-heading-respect-content)
                       (:n :v) local "<localleader>is" (evil-org-define-eol-command
                                                        org-insert-subheading)
                       (:n :v) local "<localleader>il" (evil-org-define-eol-command
                                                        org-insert-link)
                       (:n :v) local "<localleader>id" (evil-org-define-eol-command
                                                        org-insert-drawer)
                       (:n :v) local "<localleader>ip" (evil-org-define-eol-command
                                                        org-insert-property-drawer)
                       (:n :v) local "<localleader>ic" (evil-org-define-eol-command
                                                        org-insert-comment)
                       (:n :v) local "<localleader>ii" (evil-org-define-eol-command
                                                        org-insert-item)
                       (:n :i :e) local "C-<return>" #'+org/insert-item-below
                       (:n :v) local "<localleader>s" #'org-schedule
                       )))
  :config
;;;;; inserting stuff (from doom-emacs)
  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

;;;;; enable evil-org
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;; org-ref
(use-package org-ref
  :after org
  :init
  (setq	bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-additional-search-fields
        '(keywords))
  :config
  (defun p/bibtex-completion-bibliography-from-path (path)
    (require 'cl-lib)
    (cl-remove-if #'(lambda (str)
                      (not (string-match-p "\\.bib\\'" str)))
                  (directory-files path t nil t)))
  (defun p/setup-bib-completion (path)
    (setq-local bibtex-completion-bibliography
                (p/bibtex-completion-bibliography-from-path path)))
  (:maps (:n :i :e) org-mode-map (kbd "C-c r l") #'org-ref-insert-link-hydra/body
         (:n :i :e) org-mode-map (kbd "C-c r c") #'org-ref-citation-hydra/body))

;;;; toc-org
(use-package toc-org
  :hook (org-mode . toc-org-mode)
  :init
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;;;; export backends
;;;;; pandoc
(use-package ox-pandoc
  :after org
  :config (require 'ox-pandoc))
