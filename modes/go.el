;;; go.el --- Go specific configuration

;;; Commentary:
;; 

;;; Code:

(use-package go-mode
  :mode "\\.go$"
  :bind (:map go-mode-map
	      ("<s-return>" . gofmt)))

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

(use-package go-tag
  :after go-mode
  :bind (:map go-mode-map
              ("C-c t" . go-tag-add)
              ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase"))
  )

(use-package go-fill-struct
  :after go-mode
  :bind (:map go-mode-map
              ("M-RET"     . go-fill-struct)
              ("C-c f" . go-fill-struct))
  :config
  (advice-add 'go-fill-struct :after 'gofmt))

(use-package go-rename :bind ("C-c r" . go-rename))
(use-package go-impl :bind ("C-c C-c i" . go-impl))

(provide 'go)

;;; go.el ends here
