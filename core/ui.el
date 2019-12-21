(defvar after-load-theme-hook nil
  "Hook run after the color theme is loaded using `load-theme'")

(defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

(use-package doom-themes
  :ensure t
  :defer t
  :hook (after-load-theme . doom-themes-visual-bell-config)
  :config
  (setq doom-one-light-brighter-modeline t
	doom-spacegrey-brighter-modeline t
	doom-enable-italic t
	doom-enable-bold t)
  (doom-themes-org-config) 
					; (doom-themes-treemacs-config)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-init)
  :init
  (setq doom-modeline-bar-width 3
	doom-modeline-lsp t
	doom-modeline-env-version t
	doom-modeline-indent-info nil
	doom-modeline-buffer-encoding nil
	doom-modeline-major-mode-color-icon nil
	))

(use-package anzu
  :ensure t
  :config (global-anzu-mode))

(use-package kaolin-themes
  :ensure t
  :defer t
  :config (kaolin-treemacs-theme))

(use-package solaire-mode
  :ensure t
  :config (solaire-global-mode 1)
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode))

(use-package rainbow-delimiters :ensure t :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :ensure t :defer t
  :hook (prog-mode . rainbow-mode))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Default theme
(when window-system
  (load-theme 'kaolin-temple))

(provide 'ui)
