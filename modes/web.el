;;; web --- Config for Javascript, Typescript & JSX
;;; Commentary:
;;; Code:
(use-package json-mode
  :mode (("\\.json$" . json-mode)
         ("\\.eslintrc$" . json-mode))
  :config)

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :interpreter (("node" . js2-mode)
                ("node" . rjsx-mode))
  :hook (js2-mode . js2-mode-hide-warnings-and-errors)
  :config
  (setq js-indent-level 2
        js2-basic-offset 2))

(use-package rjsx-mode
  :mode ("\\.m?jsx$" . rjsx-mode)
  :hook (rjsx-mode . js2-mode-hide-warnings-and-errors)
  :config (set-face-attribute 'rjsx-tag-bracket-face nil
                              :inherit 'rjsx-tag))

(use-package auto-rename-tag
  :after rjsx-mode
  :commands (global-auto-rename-tag-mode)
  :config (global-auto-rename-tag-mode t))

(provide 'web)
;;; web.el ends here
