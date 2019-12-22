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
  :bind (
	 ("C-o" . projectile-find-file)
	 ("C-c p p" . projectile-switch-project)))

(use-package magit
  :mode ("\/COMMIT_EDITMSG$" . text-mode)
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame)
         :map magit-mode-map
         ("o" . magit-open-file-other-window)))

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

(bind-keys
 ("M-n" . forward-paragraph)
 ("M-p" . backward-paragraph)
 )

(provide 'navigation)
;;; navigation.el ends here
