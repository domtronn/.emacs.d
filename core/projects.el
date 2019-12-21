(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq
   projectile-completion-system 'ivy
   projectile-sort-order 'recently-active)
  :bind (
	 ("C-o" . projectile-find-file)
	 ("C-c p p" . projectile-switch-project)
	 )
  )

(provide 'projects)
