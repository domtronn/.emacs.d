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
  (set-face-background 'flycheck-fringe-warning (face-foreground 'flycheck-fringe-warning))
  (set-face-background 'flycheck-fringe-error (face-foreground 'flycheck-fringe-error))
  (bind-keys :prefix "C-c e"
             :prefix-map flycheck-keymap-prefix
             ("l" . flycheck-list-errors)
             ("p" . flycheck-previous-error)
             ("n" . flycheck-next-error))

  (defun flycheck-debug-eslint ()
    "Run the checker config to see what the error is."
    (interactive)
    (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
           (cmd (format "%s --print-config %s" executable (or buffer-file-name
                                                              "index.js"))))
      (message "%s" (shell-command-to-string cmd))))

  (with-eval-after-load 'js2-mode
    (setq flycheck-javascript-eslint-executable "eslint_d")))

(use-package flycheck-clojure
  :after flycheck
  :config (flycheck-clojure-setup)
  (flycheck-add-mode 'clojure-cider-kibit 'clojure-mode)
  (flycheck-add-mode 'clojure-cider-eastwood 'clojure-mode))

(use-package flycheck-rust
  :after flycheck
  :config (flycheck-rust-setup))

(use-package eslintd-fix
  :after flycheck
  :hook (js2-mode . eslintd-fix-mode))

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
  (defvar lsp-ui-non-flycheck-modes
    '(rjsx-mode js2-mode rust-mode rustic-mode))
  (setq lsp-prefer-flymake nil)

  (advice-add 'lsp-ui-flycheck-enable :after
              '(lambda (&rest args)
                 (when (memq major-mode lsp-ui-non-flycheck-modes)
                   (flycheck-disable-checker 'lsp-ui)
                   (setq-local flycheck-checker nil)))))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode)
  :bind (:map core-mode-map
              ("C-c" . whitespace-cleanup)))

(use-package format-all
  :init (define-prefix-command 'format-all-map)
  :hook ((prog-mode . format-all-mode)
         ((web-mode js2-mode) . (lambda () (format-all-mode 0))))
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
