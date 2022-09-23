;;; ranger.el --- Ranger configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up ranger to extend the built-in dired mode (configured in
;; core/dired)

;;; Code:

(use-package ranger
  :commands (deer ranger)
  :bind ([remap dired] . deer)
  :init
  (advice-add 'dired :override #'deer)
  :config
  (:maps (:n :v) global "<leader>od" #'deer
         (:n :v) global "<leader>oD" #'dired
         (:n :v) global "<leader>-" #'deer
         (:n :v) global "<leader>or" #'ranger)
  (ranger-override-dired-mode t)
  :custom
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
