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

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recently-active)
  :bind (("C-c p p" . projectile-switch-project)))

(use-package magit
  :mode ("\/COMMIT_EDITMSG$" . text-mode)
  :bind (("C-c g"            . magit-status)
         ("C-c C-g "         . magit-dispatch)
         ("C-c b"            . magit-blame)
         :map magit-mode-map
         ("o"                . magit-open-file-other-window)))

(use-package forge :after magit)

(use-package treemacs
  :bind (("s-0" . treemacs-select-window))
  :config
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

(use-package winum
  :init
  (dotimes (n 9)
    (global-set-key (kbd (format "s-%s" (1+ n)))
                    (intern (format "winum-select-window-%s" (1+ n)))))
  :config (winum-mode))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line]       . mwim-end-of-code-or-line)))

(use-package avy
  :bind ("C-'" . avy-goto-char-timer))

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

(use-package highlight-symbol
  :bind (("s->" . highlight-symbol-next)
         ("s-<" . highlight-symbol-prev)))

(bind-keys
 ("M-n"     . forward-paragraph)
 ("M-p"     . backward-paragraph)
 ("C-x C-z" . delete-other-windows)
 ("s-o"     . other-window)
 )

(provide 'navigation)
;;; navigation.el ends here
