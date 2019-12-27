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

(use-package auto-compile
  :hook (emacs-lisp-mode . auto-compile-on-save-mode))

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.

This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

(provide 'elisp)
;;; elisp.el ends here
