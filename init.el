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
  
  (eval-when-compile (require 'use-package))

  (use-package esup :ensure t :commands (esup))
  (use-package dashboard
    :ensure t
    :config
    (setq
     dashboard-startup-banner "~/.emacs.d/var/logo.png"
     dashboard-center-content t

     dashboard-set-heading-icons t
     dashboard-set-file-icons t)
    (dashboard-setup-startup-hook)
    :hook (dashboard-mode
	   . (lambda ()
	       (bind-keys
		:map dashboard-mode-map
		("n" . next-line)
		("p" . previous-line)
		("t" . counsel-load-theme)
		("f" . set-frame-font))))
    )

  (use-package no-littering :ensure t)
  (use-package try :ensure t :commands (try))

  ;; Load core modules
  (use-package ui :load-path "core")
  (use-package completion :load-path "core")
  (use-package projects :load-path "core")

  ;; ;; Load language modes
  (use-package go :load-path "modes")
  (use-package elisp :load-path "modes")
)
