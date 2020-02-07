;;; init --- My custom init file
;;; Commentary:

;; Future features / packages to look into

;; TODO: pomidor
;; TODO: docker tramp
;; TODO: kubernets porcelain
;; TODO: dired extensions
;; TODO: itunes in hydra
;; FIXME: Electric pair mode in JS is shite
;; TODO: web mode for HTML and EJS etc
;; TODO: format all popup buffer looks shit
;; FIXME: `comment-or-uncoment-region' is broken
;; TODO: Write imenu module for JSON navigation

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
    (setq use-package-always-ensure t
          use-package-enable-imenu-support t)
    (require 'use-package))

  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :commands (exec-path-from-shell-getenvs
               exec-path-from-shell-setenv)
    :config
    (setq exec-path-from-shell-debug t)
    (add-to-list 'exec-path-from-shell-variables "NVM_BIN")
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (add-to-list 'exec-path-from-shell-variables "GEM_PATH")

    :hook (emacs-startup . exec-path-from-shell-initialize))

  (use-package restart-emacs
    :commands (restart-emacs)
    :bind (("<s-kp-delete>" . kill-emacs)
           ("<S-s-kp-delete>" . restart-emacs)))

  (use-package hide-mode-line
    :hook ((dashboard-mode lsp-ui-imenu-mode help-mode helpful-mode rg-mode reb-mode) . hide-mode-line-mode))

  (use-package esup :commands (esup))
  (use-package auto-package-update
    :config
    (setq auto-package-update-interval 7
          auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
    (auto-package-update-maybe))

  (use-package no-littering)
  (use-package try :commands (try))

  (bind-keys :prefix-map core-mode-map :prefix "C-c C-c" :prefix-docstring "Clean whitespace or copy strings")
  (bind-keys :prefix-map change-view-map :prefix "C-c v" :prefix-docstring "Modify the view of the buffer")
  (bind-keys :prefix-map question-map :prefix "C-c /" :prefix-docstring "Prefix for asking questions")
  (bind-keys :prefix-map avy-map :prefix "C-x C-x" :prefix-docstring "Avy navigation")

  ;; Load core modules
  (use-package ui         :load-path "core")
  (use-package vcs        :load-path "core")
  (use-package linting    :load-path "core")
  (use-package editing    :load-path "core")
  (use-package snippets   :load-path "core")
  (use-package completion :load-path "core")
  (use-package navigation :load-path "core")

  ;; ;; Load language modes
  (use-package go      :load-path "modes")
  (use-package web     :load-path "modes")
  (use-package rust    :load-path "modes")
  (use-package ruby    :load-path "modes")
  (use-package elisp   :load-path "modes")
  (use-package clojure :load-path "modes")

  ;; custom settings
  (fset 'yes-or-no-p 'y-or-n-p)
  (bind-keys
   ("M-£"   . (lambda () (interactive) (insert "#")))
   ("M-*"   . (lambda () (interactive) (insert "•")))
   ("M-RET" . hippie-expand))
  )

(provide 'init)
;;; init.el ends here
