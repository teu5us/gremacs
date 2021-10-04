;;; comp.el --- native-comp options and stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure native compilation if it is available.

;;; Code:

(when (and (boundp 'native-comp-compiler-options)
           (boundp 'native-comp-async-report-warnings-errors))
  (when (bound-and-true-p p/native-comp-march)
    (setq native-comp-compiler-options
          '("-O2"
            (format "-march=%s"
                    p/native-comp-march)
            "-mtune=native")
          native-comp-async-report-warnings-errors nil
          native-compile-target-directory (expand-file-name "straight/eln" user-emacs-directory)))
  (push native-compile-target-directory native-comp-eln-load-path))

;;; comp.el ends here
