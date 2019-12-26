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
  :hook ((ediff-prepare-buffer . hide-mode-line-mode)
         (ediff-before-setup
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

(use-package git-messenger
  :hook (prog-mode . git-messenger:title-mode)
  :config
  (defun git-messenger:title ()
    "Get the commit to be formatted into the title."
    (let* ((vcs (git-messenger:find-vcs))
           (file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos))
           (commit-info (git-messenger:commit-info-at-line vcs file line))
           (commit-id (car commit-info))
           (commit-author (cdr commit-info))
           (commit-msg (s-trim (git-messenger:commit-message vcs commit-id))))

      (if (s-contains-p "not yet committed" commit-msg)
          (format "%s" commit-msg)
        (format "VC: %s - @%s" commit-msg commit-author))
      ))

  (define-minor-mode git-messenger:title-mode
    "Minor mode for showing git message in the frame title."
    (setq frame-title-format `(:eval (git-messenger:title))))
  )

(provide 'vcs)

;;; vcs.el ends here
