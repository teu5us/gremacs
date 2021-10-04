;;; tramp.el --- Tramp configuration -*- lexical-binding: t; -*-

(use-package tramp
  :ensure nil
  :defer 1
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; TRAMP gcloud ssh
  (add-to-list 'tramp-methods
               '("gssh"
                 (tramp-login-program        "gcloud compute ssh")
                 (tramp-login-args           (("%h")))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/bash")
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=no")))
                 (tramp-default-port         22))))
