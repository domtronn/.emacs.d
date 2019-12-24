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
  :bind (:map vc-prefix-map
         ("v" . browse-at-remote)
         :map magit-mode-map
         ("V" . browse-at-remote)))

(use-package git-link
  :bind (:map vc-prefix-map
              ("l" . git-link)))

(use-package ediff
  :ensure nil
  :init (defvar ediff-window-config nil)
  :bind (:map vc-prefix-map ("d" . vc-ediff))
  :hook ((ediff-before-setup
          . (lambda () (setq ediff-window-config (current-window-configuration))))
         (ediff-quit
          . (lambda ()
              (set-window-configuration ediff-window-config)
              (-map 'kill-buffer
                        (--filter (and (not (string-equal (buffer-name) (buffer-name it)))
                                       (s-starts-with-p (buffer-name) (buffer-name it)))
                                  (buffer-list))))))
  :config
  (setq
   ediff-keep-variants t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'vcs)

;;; vcs.el ends here
