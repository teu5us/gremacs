;;; ibuffer.el --- Ibuffer configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up ibuffer to replace the built-in functionality for
;; interactive buffer manipulation.

;;; Code:

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
	   (name 35 35 :left :elide)
	   " "
	   (size 9 -1 :right)
	   " "
	   (mode 16 16 :left :elide)
	   " " filename-and-process)
     (mark " "
	   (name 16 -1)
	   " " filename))))

(use-package ibuffer-vc
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote 'nil))
