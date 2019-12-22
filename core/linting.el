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

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

(use-package format-all
  :init (define-prefix-command 'format-all-map)
  :config
  (bind-keys :prefix "C-c RET"
             :prefix-map format-all-map
             ("RET" . format-all-buffer)))

(use-package quickrun
  :commands quickrun
  :init (define-prefix-command 'quickrun-map)
  :config
  (bind-keys ("C-x RET RET" . quickrun)))

(provide 'linting)
;;; linting.el ends here
