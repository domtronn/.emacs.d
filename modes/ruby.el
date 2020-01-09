;;; ruby.el --- Ruby specific configuration

;;; Commentary:
;;

;;; Code:

(use-package ruby-mode
  :mode "\\.rb$")

(use-package enh-ruby-mode
  :hook (ruby-mode . enh-ruby-mode) )

(use-package haml-mode
  :mode "\\.haml$")

(provide 'ruby)
;;; ruby.el ends here
