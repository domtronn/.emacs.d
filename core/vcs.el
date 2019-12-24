;;; vcs.el --- Configuration for version control packages

;;; Commentary:
;;

;;; Code:
(use-package magit
  :mode ("\/COMMIT_EDITMSG$" . text-mode)
  :bind (("C-c g"            . magit-status)
         ("C-c C-g "         . magit-dispatch)
         ("C-c b"            . magit-blame)
         :map magit-mode-map
         ("o"                . magit-open-file-other-window)))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package forge :after magit)

(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

(use-package browse-at-remote
  :bind (("C-c v" . browse-at-remote)
         :map magit-mode-map
         ("V" . browse-at-remote)))

(provide 'vcs)

;;; vcs.el ends here
