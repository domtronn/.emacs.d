;;; linting --- Configuration for code excellence and linting
;;; Commentary:
;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :commands (flycheck-list-errors
             flycheck-previous-error
             flycheck-next-error)
  :bind (("M-}" . flycheck-mode))
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-disabled-checkers '(javscript-jshint scss))
  (bind-keys :prefix "C-c e"
             :prefix-map flycheck-keymap-prefix
             ("l" . flycheck-list-errors)
             ("p" . flycheck-previous-error)
             ("n" . flycheck-next-error))

  (with-eval-after-load 'js2-mode
    (setq flycheck-javascript-eslint-executable "eslint_d")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (prog-mode . lsp-ui-mode)
  :config
  (setq lsp-prefer-flymake nil)

  (advice-add 'lsp-ui-flycheck-enable :around
              '(lambda (orig-f &rest args)
                 (let ((current-checker (flycheck-get-checker-for-buffer)))
                   (apply orig-f args)
                   (flycheck-select-checker current-checker)))))

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
