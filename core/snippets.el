;;; snippets.el --- A collection of yasnippet configs

;;; Commentary:
;;

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package go-snippets
  :after (yasnippet go-mode))

(use-package js-react-redux-yasnippets :after (js2-mode yasnippet))
(use-package react-snippets :after (js2-mode yasnippet))

(provide 'snippets)

;;; snippets.el ends here
