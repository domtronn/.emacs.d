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
   dashboard-footer-icon (if (not (display-graphic-p)) "♡"
                           (all-the-icons-faicon "heart" :height 1.1 :v-adjust -0.05 :face 'error))

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
       (lambda (&rest _) (call-interactively 'ivy-set-font)))
      )))

  (dashboard-setup-startup-hook)
  :hook (dashboard-mode
         . (lambda ()
             (bind-keys
              :map dashboard-mode-map
              ("SPC" . widget-forward)
              ("C-n" . next-line)
              ("C-p" . previous-line)
              ("t"   . counsel-load-theme)
              ("f"   . ivy-set-font)))))

(use-package kaolin-themes :defer t
  :custom (kaolin-themes-underline-wave nil))

(use-package doom-themes
  :hook ((after-load-theme . doom-themes-visual-bell-config)
         (after-load-theme . doom-themes-treemacs-config))
  :config
  (doom-themes-org-config)
  (with-no-warnings
    (setq doom-one-light-brighter-modeline t
          doom-spacegrey-brighter-modeline t
          doom-enable-italic t
          doom-enable-bold t)))

(use-package frame
  :after solaire-mode
  :ensure nil
  :hook (after-load-theme
         . (lambda ()
             (set-face-foreground 'window-divider (face-background 'default))
             (set-face-foreground 'vertical-border (face-background 'default))))
  :config
  (set-face-foreground 'window-divider (face-background 'default))
  (set-face-foreground 'vertical-border (face-background 'default))
  (setq window-divider-default-right-width 0
        window-divider-default-bottom-width 0))

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
        doom-modeline-major-mode-color-icon nil)

  :config
  (set-face-attribute 'region nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))
  (set-face-attribute 'highlight nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))

  (eval-when-compile
    'company
    (doom-modeline-def-segment company-backend
      "Display the current company backend. `company-backend'."
      (when (company--active-p)
        (format "%s"
                (--map (s-replace "company-" "" (format "%s" it))
                       (if (listp company-backend) company-backend (list company-backend)))))))

  (doom-modeline-def-segment buffer-info
    "Overwrite of buffer info to not include the icon"
    (concat
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment buffer-type
    "Buffer icon and version if it exists"
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (when (and doom-modeline-env-version doom-modeline-env--version)
       (propertize
        (format "%s " doom-modeline-env--version)
        'face '(:height 0.7)))))

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-type buffer-info remote-host buffer-position word-count selection-info)
    '(company-backend misc-info persp-name battery debug lsp input-method buffer-encoding  process vcs checker)))

(use-package shackle
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.3
        shackle-default-alignment 'below
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          (("*compilation*" "*Compile-Log*") :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          (("*shell*" "*eshell*" "*ielm*") :popup t :align 'below)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)

          (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
          ((flycheck-error-list-mode point-history-show-mode flymake-diagnostics-buffer-mode) :select t :size 0.3 :align 'below :autoclose t)

          ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t))))

(use-package anzu :config (global-anzu-mode))

(use-package solaire-mode
  :config (solaire-global-mode 1)
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (after-load-theme . solaire-mode-swap-bg)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package focus
  :bind (("H-f" . focus-mode)
         :map change-view-map
         ("f" . focus-mode)))

(use-package olivetti
  :init
  (defvar olivetti--display-linum nil)
  (defvar olivetti--hide-mode-line nil)
  (defvar olivetti-enter-mode-hook nil "Hook run when entering `olivetti-mode'.")
  (defvar olivetti-exit-mode-hook nil "Hook run when exiting `olivetti-mode'.")
  :config
  (advice-add 'olivetti-mode
              :after (lambda (&rest _) (if olivetti-mode
                                      (run-hooks 'olivetti-enter-mode-hook)
                                    (run-hooks 'olivetti-exit-mode-hook))))

  :hook ((olivetti-exit-mode
          . (lambda ()
              (hide-mode-line-mode (or olivetti--hide-mode-line 0))
              (display-line-numbers-mode olivetti--display-linum)))
         (olivetti-enter-mode
          . (lambda ()
              (setq-local olivetti--display-linum display-line-numbers-mode)
              (setq-local olivetti--hide-mode-line hide-mode-line-mode)
              (display-line-numbers-mode 0)
              (hide-mode-line-mode 1))))
  :bind (("H-w" . olivetti-mode)
         :map change-view-map
         ("w" . olivetti-mode)))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :defer t
  :hook ((prog-mode . rainbow-mode)
         (help-mode . rainbow-mode)))

(use-package beacon
  :after doom-modeline
  :bind (:map change-view-map
              ("v" . beacon-blink)
              ("b" . beacon-mode))
  :config (setq beacon-color (face-background 'doom-modeline-bar)))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (setq display-line-numbers-width 3)
  (let ((bg (face-attribute 'solaire-default-face :background)))
    (set-face-attribute 'line-number nil :height 0.7 :background bg)
    (set-face-attribute 'line-number-current-line nil :height 0.7 :background bg)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Default theme
(load-theme (if window-system 'kaolin-dark 'doom-moonlight))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)))

(defvar ts-default 120 "Default amount for text to be scaled.")
(defvar ts-factor 1.05 "Amount to scale text per step.")

(cl-defmacro deffacescale (face)
  `(progn
     (defun ,(intern (format "%s-ts-increase" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height (truncate (* ts-factor (face-attribute ',face :height nil 'default) ))))

     (defun ,(intern (format "%s-ts-reset" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height ts-default))

     (defun ,(intern (format "%s-ts-decrease" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height (truncate (/ (face-attribute ',face :height nil 'default) ts-factor))))))

(deffacescale default)
(deffacescale solaire-default-face)
(deffacescale variable-pitch)

(bind-keys ("H-0" . solaire-default-face-ts-reset)
           ("H--" . solaire-default-face-ts-decrease)
           ("H-=" . solaire-default-face-ts-increase))

;; Custom sets
(display-battery-mode 1)

(fringe-mode 0)
(set-window-fringes nil 0 0)
(setq-default use-file-dialog nil
              use-dialog-box nil
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t

              truncate-lines t

              ns-use-thin-smoothing t
              ns-antialias-text t
              ns-pop-up-frames nil)

(provide 'ui)
;;; ui.el ends here
