;;; editing --- Tools that make editing files generally easier
;;; Commentary:
;;; Code:
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char)
         ("C-S-d" . smart-hungry-delete-backward-char))
  :config (smart-hungry-delete-add-default-hooks))

(use-package move-text
  :bind (("s-P" . move-text-up)
         ("s-N" . move-text-down)))

(provide 'editing)
;;; editing.el ends here
