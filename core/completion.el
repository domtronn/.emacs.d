;;; completion --- Configuration for code completion
;;; Commentary:
;;; Code:

(use-package snails
  :if (memq window-system '(mac ns))
  :load-path "etc/elisp-packages/snails"
  :bind ("s-SPC" . snails))

(use-package amx
  :init
  (setq amx-history-length 20
        amx-show-key-bindings t))

(use-package company
  :config
  (setq company-show-numbers t
        company-tooltip-align-annotations t
        )
  :hook (after-init     . global-company-mode)
  :bind (("<kp-enter>"  . company-complete)
         ("M-/"         . company-complete)
         :map company-active-map
         ("C-n"         . company-select-next)
         ("C-p"         . company-select-previous)
         ))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        ))

(use-package counsel
  :after ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("s-Y"     . counsel-yank-pop))
  :config
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n----------\n"
      ))

(use-package counsel-projectile
  :after (projectile counsel)
  :bind (("C-x C-b" . counsel-projectile-swnnnitch-to-buffer)))

(use-package counsel-osx-app
  :if (memq window-system '(mac ns))
  :bind (("s-O" . counsel-osx-app)))

(use-package swiper
  :bind (("C-;" . swiper)
         ("C-:" . swiper-all)))

(use-package ivy-hydra :after ivy)
(use-package ivy
  :defer 1
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  :bind (:map ivy-minibuffer-map
         ("<backspace>" . delete-backward-char)
         ("C-d"         . delete-forward-char)))

(use-package ivy-posframe
  :if window-system
  :after ivy
  :config
  (setq ivy-posframe-border-width 20
        ivy-posframe-min-width 60
        ivy-posframe-width 120
        ivy-posframe-min-height 20
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode))

;; LSP servers
;; go - GO111MODULE=on go get golang.org/x/tools/gopls@latest

(use-package lsp-mode
  :hook ((go-mode  . lsp-deferred)
         (js2-mode . lsp-deferred))
  :commands (lsp))

(use-package company-lsp
  :config (push 'company-lsp company-backends))

(provide 'completion)
;;; completion.el ends here
