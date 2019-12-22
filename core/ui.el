;;; completion --- UI configuration
;;; Commentary:
;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after the color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package kaolin-themes :defer t)

(use-package doom-themes
  :hook ((after-load-theme . doom-themes-visual-bell-config)
         (after-load-theme . doom-themes-treemacs-config))
  :config
  (doom-themes-org-config)
  (with-no-warnings
    (setq doom-one-light-brighter-modeline t
          doom-spacegrey-brighter-modeline t
          doom-enable-italic t
          doom-enable-bold t))
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-lsp t
        doom-modeline-env-version t
        doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 20
        doom-modeline-major-mode-color-icon nil
        ))

(use-package anzu :config (global-anzu-mode))

(use-package solaire-mode
  :config (solaire-global-mode 1)
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :defer t
  :hook (prog-mode . rainbow-mode))

(use-package popwin

  )

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width 3)
  (let ((bg (face-attribute 'solaire-default-face :background)))
    (set-face-attribute 'line-number nil :height 0.7 :background bg)
    (set-face-attribute 'line-number-current-line nil :height 0.7 :background bg)))


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Default theme
(load-theme
 (if window-system
     'kaolin-temple
   'doom-moonlight))

;; Custom sets
(setq-default use-file-dialog nil
              use-dialog-box nil
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t

              fringe-mode 0
              truncate-lines t

              ns-use-thin-smoothing t
              ns-antialias-text t
              ns-pop-up-frames nil)

(provide 'ui)
;;; ui.el ends here
