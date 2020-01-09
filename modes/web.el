;;; web --- Config for Javascript, Typescript & JSX
;;; Commentary:
;;; Code:

;; HTML
(use-package web-mode
  :mode (("\\.ejs$" . web-mode)
         ("\\.html$" . web-mode))
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

;; JS
(use-package json-mode
  :mode (("\\.json$" . json-mode)
         ("\\.eslintrc$" . json-mode))
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.m?js$" . js2-mode))
  :interpreter (("node" . js2-mode)
                ("node" . rjsx-mode))
  :hook (js2-mode . js2-mode-hide-warnings-and-errors)
  :config
  (unbind-key "M-." js2-mode-map)
  (setq js-indent-level 2
        js2-basic-offset 2))

(use-package rjsx-mode
  :mode ("\\.m?jsx?$" . rjsx-mode)
  :hook (rjsx-mode . js2-mode-hide-warnings-and-errors)
  :config
  (set-face-attribute 'rjsx-tag-bracket-face nil
                      :inherit 'rjsx-tag)
  (unbind-key ">" rjsx-mode-map))

(use-package auto-rename-tag
  :after rjsx-mode
  :commands (global-auto-rename-tag-mode)
  :config (global-auto-rename-tag-mode t))

(use-package js-import
  :after js2-mode
  :custom
  (js-import-quote "'")
  :bind ((:map js2-mode-map
               ("C-c i" . js-import)
               ("s-i"   . js-import))))

(with-eval-after-load 'doom-modeline
  (doom-modeline-def-env node
    :hooks   '(js2-mode-hook rjsx-mode-hook javascript-mode-hook)
    :command (lambda () (list "node" "--version"))
    :parser  (lambda (line) (s-join "." (butlast (split-string (cadr (split-string (s-trim line) "v")) "\\.") 1)))))

;; CSS & SCSS
(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package scss-mode
  :init (setq scss-compile-at-save nil))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode) . turn-on-css-eldoc))


(provide 'web)
;;; web.el ends here
