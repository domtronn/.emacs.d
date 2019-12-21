(use-package highlight-defined :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted :ensure t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-parentheses :ensure t
  :hook (emacs-lisp-mode . highlight-parentheses-mode)
  :config (setq hl-paren-colors '("#91ca55" "#f45d43" "#ee3f46" "#c0392b")))


(provide 'elisp)
