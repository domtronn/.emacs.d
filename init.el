;;; init --- My custom init file
;;; Commentary:

;; Future features / packages to look into

;; TODO: focus
;; TODO: howdoyou
;; TODO: pomidor
;; TODO: docker tramp
;; TODO: kubernets porcelain

;;; Code:

(let ((file-name-handler-alist nil))
  ;; Set GC threshold to speed up start time
  (setq gc-cons-threshold 100000000)

  ;; Init files
  (setq-default user-init-file "~/.emacs.d/init.el")
  (setq-default custom-file "~/.emacs.d/custom.el")
  (setq-default package-user-dir "~/.emacs.d/etc/packages")
  (load-file "~/.emacs.d/custom.el")

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

  (use-package restart-emacs :commands (restart-emacs))

  (use-package hide-mode-line
    :hook ((dashboard-mode lsp-ui-imenu-mode) . hide-mode-line-mode))

  (use-package esup :commands (esup))
  (use-package spu
    :defer 5
    :config (spu-package-upgrade-daily))

  (use-package no-littering)
  (use-package try :commands (try))

  ;; Load core modules
  (use-package ui         :load-path "core")
  (use-package vcs        :load-path "core")
  (use-package linting    :load-path "core")
  (use-package editing    :load-path "core")
  (use-package completion :load-path "core")
  (use-package navigation :load-path "core")

  ;; ;; Load language modes
  (use-package go    :load-path "modes")
  (use-package web   :load-path "modes")
  (use-package elisp :load-path "modes")

  ;; custom settings
  (fset 'yes-or-no-p 'y-or-n-p)
  (bind-keys
   ("M-£" . (lambda () (interactive) (insert "#")))
   ("M-*" . (lambda () (interactive) (insert "•"))))
  )

(provide 'init)
;;; init.el ends here
