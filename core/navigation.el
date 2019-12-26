;;; completion --- Libraries for improving navigation around code
;;; Commentary:
;;; Code:

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recently-active)
  :bind (("C-c p p" . projectile-switch-project)))

(use-package treemacs
  :bind (("s-0" . treemacs-select-window))
  :config
  (advice-add 'doom-themes-hide-fringes :after (lambda () (set-window-fringes nil 6 0)))
  (with-no-warnings
    (treemacs-follow-mode)
    (treemacs-filewatch-mode)
    (treemacs-fringe-indicator-mode)
    (treemacs-git-mode 'simple)
    (setq treemacs-is-never-other-window t
          treemacs-position 'right))
  )

(use-package treemacs-magit :after treemacs magit)
(use-package treemacs-projectile :after treemacs projectile)

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        `(("."      . ,(locate-user-emacs-file "var/undo-tree-hist/"))))
  :bind (("C-c C-u" . undo-tree-visualize)
         ("C-_"     . undo-tree-undo)
         ("C-+"     . undo-tree-redo)))

(use-package winum
  :init
  (dotimes (n 9)
    (global-set-key (kbd (format "s-%s" (1+ n)))
                    (intern (format "winum-select-window-%s" (1+ n)))))
  :config (winum-mode))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line]       . mwim-end-of-code-or-line)))

(use-package link-hint
  :bind ("H-l" . link-hint-open-link))

(defun --set-face (face fg)
  "Rebind FACE to be FG on background FG."
  (set-face-attribute face nil :bold t
                      :background (face-foreground fg)
                      :foreground (face-background 'default)))

(use-package avy
  :config
  (avy-setup-default)
  (eval-after-load 'ivy
    (progn
      (--set-face 'avy-lead-face 'ivy-minibuffer-match-face-2)
      (--set-face 'avy-lead-face-0 'ivy-minibuffer-match-face-3)
      (--set-face 'avy-lead-face-1 'ivy-minibuffer-match-face-4)
      (--set-face 'avy-lead-face-2 'ivy-minibuffer-match-face-1)))

  :bind (("C-'" . avy-goto-char-timer)
         ("C-S-l" . avy-goto-line)))

(use-package avy-flycheck
  :bind ("C-c '" . avy-flycheck-goto-error))

(use-package dumb-jump
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy)
  :bind (("M-." . dumb-jump-go)
         ("M-?" . dumb-jump-go-prompt)
         ("M-," . dumb-jump-back)))

(use-package goto-chg
  :bind (("s-," . goto-last-change)
         ("s-." . goto-last-change-reverse)))

(use-package wgrep
  :hook (rg-mode . wgrep-rg-setup)
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-package rg
  :defer t
  :config
  (rg-enable-default-bindings)
  (setq rg-executable "/usr/local/bin/rg"
        rg-show-columns t
        rg-show-header t
        rg-group-result nil)
  (bind-keys
   :map rg-mode-map
   ("W" . wgrep-change-to-wgrep-mode)))

(use-package auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (--set-face 'ahs-plugin-defalt-face 'ivy-minibuffer-match-face-2)
  (--set-face 'ahs-definition-face 'ivy-minibuffer-match-face-1)
  (--set-face 'ahs-face 'ivy-minibuffer-match-face-1))

(use-package highlight-symbol
  :bind (("s->" . highlight-symbol-next)
         ("s-<" . highlight-symbol-prev)))

(bind-keys
 ("M-n"     . forward-paragraph)
 ("M-p"     . backward-paragraph)
 ("C-x C-z" . delete-other-windows)
 ("s-o"     . other-window)

 ("C-x C-3" . (lambda () (interactive) (split-window-right) (other-window 1)))
 ("C-x C-2" . (lambda () (interactive) (split-window-below) (other-window 1)))

 )

(provide 'navigation)
;;; navigation.el ends here
