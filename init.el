;;; init --- My custom init file
;;; Commentary:
;;; Code:

(let ((file-name-handler-alist nil))
  ;; Set GC threshold to speed up start time
  (setq gc-cons-threshold 100000000)

  ;; Init files
  (setq-default user-init-file "~/.emacs.d/init.el")
  (setq-default custom-file "~/.emacs.d/custom.el")
  (setq-default package-user-dir "~/.emacs.d/etc/packages")
  (load-file "~/.emacs.d/custom.el")

  ;; Custom variables
  (setq

   )

  ;; Bootup mode
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (eval-when-compile
    (require 'use-package)
    (setq use-package-always-ensure t))

  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :commands (exec-path-from-shell-getenvs
               exec-path-from-shell-setenv)
    :hook (emacs-startup
           . (lambda ()
               (async-start
                `(lambda () '(,(exec-path-from-shell-getenvs exec-path-from-shell-variables)))
                (lambda (res) (mapc (lambda (p) (exec-path-from-shell-setenv (car p) (cdr p))) (car res)))))))

  (use-package hide-mode-line
    :hook ((dashboard-mode) . hide-mode-line-mode))

  (use-package esup :commands (esup))
  (use-package dashboard
    :config
    (setq
     dashboard-startup-banner "~/.emacs.d/var/logo.png"
     dashboard-center-content t

     dashboard-set-heading-icons t
     dashboard-set-file-icons t

     dashboard-items '((recents . 5)
                       (projects . 5)
                       (bookmarks . 5)))

    (dashboard-setup-startup-hook)
    :hook (dashboard-mode
           . (lambda ()
               (bind-keys
                :map dashboard-mode-map
                ("C-n" . next-line)
                ("C-p" . previous-line)
                ("t" . counsel-load-theme)
                ("f" . set-frame-font)))))

  (use-package no-littering)
  (use-package try :commands (try))

  ;; Load core modules
  (use-package ui :load-path "core")
  (use-package completion :load-path "core")
  (use-package navigation :load-path "core")
  (use-package linting :load-path "core")
  (use-package editing :load-path "core")

  ;; ;; Load language modes
  (use-package go :load-path "modes")
  (use-package web :load-path "modes")
  (use-package elisp :load-path "modes")

  ;; Custom settings
  (fset 'yes-or-no-p 'y-or-n-p))

(provide 'init)
;;; init.el ends here
