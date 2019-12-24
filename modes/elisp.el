;;; elisp.el --- Configuration when working in emacs-lisp-mode

;;; Commentary:
;; 

;;; Code:

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-parentheses
  :hook (emacs-lisp-mode . highlight-parentheses-mode)
  :config (setq hl-paren-colors '("#91ca55" "#f45d43" "#ee3f46" "#c0392b")))

(use-package eros
  :hook (emacs-lisp-mode      . eros-mode)
  :bind (:map emacs-lisp-mode-map
              ("<s-return>"   . eros-eval-last-sexp)
              ("<s-S-return>" . eros-eval-defun))
  :config
  (set-face-attribute 'eros-result-overlay-face nil
                      :box `(:line-width -1 :color ,(face-attribute 'success :foreground))
                      :foreground (face-attribute 'success :foreground)
                      :inherit 'solaire-default-face))

(provide 'elisp)
;;; elisp.el ends here
