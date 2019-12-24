;;; editing --- Tools that make editing files generally easier
;;; Commentary:
;;; Code:
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d"         . smart-hungry-delete-forward-char)
         ("C-S-d"       . smart-hungry-delete-backward-char))
  :config (smart-hungry-delete-add-default-hooks))

(use-package smart-newline
  :bind ("RET" . smart-newline))

(use-package move-text
  :bind (("s-P" . move-text-up)
         ("s-N" . move-text-down)))

(use-package smartparens-config
  :ensure nil :after smartparens)

(use-package smartparens
  :commands (smartparens)
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode))
  :bind (("s-e" . sp-end-of-sexp)
         ("s-a" . sp-beginning-of-sexp)

         ("s-f" . sp-forward-sexp)
         ("s-b" . sp-backward-sexp)

         ("s-p" . sp-backward-up-sexp)
         ("s-n" . sp-backward-down-sexp)

         ("<s-backspace>"   . sp-splice-sexp)
         ("<S-s-backspace>" . sp-backward-kill-sexp)
         ("s--"             . sp-forward-slurp-sexp)

         :map emacs-lisp-mode-map
         ("C-K" . sp-kill-whole-line)))

(use-package embrace
  :bind (("C-," . embrace-add)
         ("C-<" . embrace-change)))

(use-package expand-region
  :bind ("M-q" . er/expand-region)
  :defines (er/copy-string er/copy-symbol)
  :commands (er/mark-symbol er/mark-inside-quotes)
  :config
  (defmacro defcopy (name f)
    `(defun ,(intern (format "er/copy-%s" name)) ()
       (interactive)
       (save-excursion
         (call-interactively ',f)
         (kill-ring-save (region-beginning) (region-end)))))

  (defcopy "string" er/mark-inside-quotes)
  (defcopy "symbol" er/mark-symbol)

  (bind-keys :prefix "C-c C-c"
             :prefix-map clean-copy-map
             ("q" . er/copy-string)
             ("s" . er/copy-symbol)))

(use-package electric-operator
  :commands (electric-operator-get-rules-for-mode
             electric-operator-add-rules-for-mode)
  :hook (prog-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode
   'emacs-lisp-mode
   (cons "-" nil)
   (cons "." " . ")))

(use-package visual-regexp-steroids
  :bind (("s-r" . vr/replace)
         ("s-R" . vr/query-replace)))

(use-package duplicate-thing
  :config
  (defun duplicate-thing-replace ()
    (interactive)
    (call-interactively 'duplicate-thing)
    (call-interactively 'vr/query-replace))

  :bind (("s-d" . duplicate-thing)
         ("s-D" . duplicate-thing-replace)))

(use-package multiple-cursors
  :bind (("<down>"   . mc/mark-next-like-this)
         ("<S-down>" . mc/skip-to-next-like-this)
         ("<up>"     . mc/mark-previous-like-this)
         ("<S-up>"   . mc/skip-to-previous-like-this)
         ("<right>"  . mc/mark-all-like-this)
         ("<left>"   . mc/edit-lines)
         :map mc/keymap
         ("C-|"      . mc/vertical-align-with-space)
         ("C-i"      . mc/insert-numbers)))

(use-package multi-line
  :bind (("C-c [" . multi-line-single-line)
         ("C-c ]" . multi-line)))

(use-package kmacro
  :ensure nil
  :bind (("H-["   . kmacro-start-macro)
         ("H-r"   . kmacro-start-macro)
         ("H-]"   . kmacro-end-macro)
         ("H-SPC" . kmacro-end-or-call-macro)
         ("H-e"   . kmacro-end-or-call-macro)))

(delete-selection-mode 1)

(bind-keys
 ("C-K" . kill-whole-line)
 ("M-D" . backward-kill-word)
 ("s-/" . comment-or-uncomment-region)
 ("C-j" . join-line))

(provide 'editing)
;;; editing.el ends here
