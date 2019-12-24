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
  :bind (("C-." . lsp-ui-imenu)
         :map lsp-ui-imenu-mode-map
         ("n"   . next-line)
         ("p"   . previous-line)
         ("M-n" . lsp-ui-imenu--next-kind)
         ("M-p" . lsp-ui-imenu--prev-kind))
  :config
  (defvar
    lsp-ui-non-flycheck-modes
    '(rjsx-mode js2-mode))
  (setq lsp-prefer-flymake nil)

  (advice-add 'lsp-ui-flycheck-enable :after
              '(lambda (&rest args)
                 (flycheck-disable-checker 'lsp-ui)
                 (setq-local flycheck-checker nil))))

(use-package whitespace-cleanup-mode
  :init (define-prefix-command 'clean-copy-map)
  :config
  (global-whitespace-cleanup-mode)
  (bind-keys :prefix "C-c C-c"
             :prefix-map clean-copy-map
             ("C-c" . whitespace-cleanup)))

(use-package format-all
  :init (define-prefix-command 'format-all-map)
  :config
  (bind-keys :prefix "C-c RET"
             :prefix-map format-all-map
             ("RET" . format-all-buffer)))

(use-package quickrun
  :commands quickrun
  :bind ("C-x RET RET" . quickrun)
  )

(provide 'linting)
;;; linting.el ends here
