;;; clojure.el --- Configuration specific to clojure

;;; Commentary:
;;

;;; Code:
(use-package clojure-mode
  :hook (clojure-mode . eldoc-mode)
  :mode ("\\.clj$" . clojure-mode)
  :bind (:map clojure-mode-map))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :commands (cider-mode cider-jack-in)
  :config
  (setq nrepl-log-messages nil
        nrepl-sync-request-timeout 60
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-display-help-banner nil)
  :bind (:map
         cider-repl-mode-map
         ("C-r"    . cider-repl-history)
         ("<up>"   . cider-repl-previous-input)
         ("<down>" . cider-repl-next-input)
         :map
         cider-mode-map
         ("<s-return>"   . cider-eval-last-sexp)
         ("<s-S-return>" . cider-eval-defun-at-point)
         ("C-h C-/"      . cider-inspect-last-result)
         ("M-;"          . cider-read-and-eval-defun-at-point)
         ("M-:"          . eval-expression)))

(use-package flycheck-clojure
  :after flycheck
  :config (flycheck-clojure-setup)
  (flycheck-add-mode 'clojure-cider-kibit 'clojure-mode)
  (flycheck-add-mode 'clojure-cider-eastwood 'clojure-mode))

(provide 'clojure)
;;; clojure.el ends here
