(use-package go-mode
  :ensure t
  :mode "\\.go$"
  :bind (:map go-mode-map
	      ("<s-return>" . gofmt)))

(use-package go-eldoc
  :ensure t
  :after go-mode
  :hook (go-mode . go-eldoc-setup))
  
(provide 'go)
