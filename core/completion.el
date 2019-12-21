(use-package exec-path-from-shell
  :ensure t
  :defer t)

(use-package snails
  :load-path "etc/elisp-packages/snails"
  :bind ("s-SPC" . snails))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-show-numbers t)
  :bind (("<kp-enter>" . company-complete)
	 ;; Bindings for navigating company
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ))

(use-package smex
  :ensure t
  :after ivy)

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x"     . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(use-package ivy-hydra :ensure t :after ivy)
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq 
        ivy-height 9
	ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy))))

;; LSP servers
;; go - GO111MODULE=on go get golang.org/x/tools/gopls@latest

(use-package lsp-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :commands (lsp)
  )

(provide 'completion)
