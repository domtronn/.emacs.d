;;; markdown.el --- Markdown specific configuration

;;; Commentary:
;;

;;; Code:

(use-package markdown-mode
  :ensure nil
  :mode "\\.md$"
  :hook (markdown-mode . auto-fill-mode)
  :bind (:map markdown-mode-map
              ("M-," . fill-paragraph)
              ("s-k" . markdown-insert-link)
              ("s-i" . markdown-insert-italic)
              ("s-b" . markdown-insert-bold)))

(use-package grip-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("r" . markdown-toc-generate-or-refresh-toc)))

(provide 'markdown)
;;; markdown.el ends here
