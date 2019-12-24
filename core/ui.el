;;; completion --- UI configuration
;;; Commentary:
;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after the color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package dashboard
  :config
  (setq
   dashboard-startup-banner "~/.emacs.d/var/logo.png"
   dashboard-center-content t

   dashboard-set-heading-icons t
   dashboard-set-file-icons t

   dashboard-items '((recents . 10)
                     (projects . 5)
                     (bookmarks . 5))

   dashboard-footer "Lovingly handrolled"
   dashboard-footer-icon (if (display-graphic-p)
                           (all-the-icons-faicon "heart"
                                                 :height 1.1
                                                 :v-adjust -0.05
                                                 :face 'error) "♡")

   dashboard-set-navigator (display-graphic-p)
   dashboard-navigator-buttons
   `(((,(when (display-graphic-p)
          (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
       "Homepage" "Browse homepage"
       (lambda (&rest _) (browse-url-chrome "https://github.com")))

      (,(when (display-graphic-p)
          (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
       "Settings" "Open custom file"
       (lambda (&rest _) (find-file user-init-file)))

      (,(when (display-graphic-p)
          (all-the-icons-faicon "paint-brush" :height 1.0 :v-adjust 0.0))
       "Theme" "Change the theme"
       (lambda (&rest _) (counsel-load-theme)))

      (,(when (display-graphic-p)
          (all-the-icons-fileicon "font" :height 1.0 :v-adjust 0.0))
       "Font" "Change the font"
       (lambda (&rest _) (call-interactively 'set-frame-font)))

      ))
   )


  (dashboard-setup-startup-hook)
  :hook (dashboard-mode
         . (lambda ()
             (bind-keys
              :map dashboard-mode-map
              ("C-n" . next-line)
              ("C-p" . previous-line)
              ("t" . counsel-load-theme)
              ("f" . set-frame-font)))))

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
        doom-modeline-major-mode-color-icon nil))

(use-package shackle
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  )

(use-package anzu :config (global-anzu-mode))

(use-package solaire-mode
  :config (solaire-global-mode 1)
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer)
         (after-load-theme . solaire-mode-swap-bg)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :defer t
  :hook (prog-mode . rainbow-mode))

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

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode))

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
