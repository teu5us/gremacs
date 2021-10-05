;;; ranger.el --- Ranger configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up ranger to extend the built-in dired mode (configured in
;; core/dired)

;;; Code:

(use-package ranger
  :commands (deer ranger)
  :config
  (:maps (:n :v) global "<leader>od" #'deer
         (:n :v) global "<leader>oD" #'dired
         (:n :v) global "<leader>-" #'deer
         (:n :v) global "<leader>or" #'ranger)
  :custom
  (ranger-override-dired-mode t)
  (ranger-cleanup-on-disable t)
  (ranger-cleanup-eagerly t)
  (ranger-modify-header t)
  (ranger-hide-cursor t)
  (ranger-preview-file nil)
  (ranger-max-preview-size 70)
  (ranger-show-literal nil)
  (ranger-dont-show-binary t)
  (ranger-excluded-extensions '("mkv" "mp4" "mp3" "ogg" "avi"
                                "core" "iso" "zip" "raw" "qcow2"
                                "tar" "tar.gz" "tar.bz2" "tar.xz"
                                "pdf" "doc" "docx" "xls" "xlsx"
                                "ppt" "pptx" "odt" "ods" "odp"
				                "msi" "exe" "AppImage")))
