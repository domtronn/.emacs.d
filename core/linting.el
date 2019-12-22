;;; linting --- Configuration for code excellence and linting
;;; Commentary:
;;; Code:

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :commands (flycheck-list-errors
             flycheck-previous-error
             flycheck-next-error)
  :config
  (global-flycheck-mode)
  (bind-keys :prefix "C-c e"
             :prefix-map flycheck-keymap-prefix
             ("l" . flycheck-list-errors)
             ("p" . flycheck-previous-error)
             ("n" . flycheck-next-error))
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (prog-mode . lsp-ui-mode)
  :config
  (setq lsp-prefer-flymake nil))

(provide 'linting)
;;; linting.el ends here
