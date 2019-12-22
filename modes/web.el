;;; web --- Config for Javascript, Typescript & JSX
;;; Commentary:
;;; Code:
(use-package json-mode
  :mode (("\\.json$" . json-mode)
         ("\\.eslintrc$" . json-mode))
  :config)

(use-package js2-mode
  :mode (("\\.jsx?$" . js2-mode))
  :config
  (setq js-indent-level 2
        js2-basic-offset 2))

(provide 'js)
;;; web.el ends here
