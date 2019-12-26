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
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-idle-delay 0
        company-minimum-prefix-length 3
        )
  :hook (after-init    . global-company-mode)
  :bind (("<kp-enter>" . company-complete)
         ("M-/"        . company-complete)
         :map company-active-map
         ("<tab>"      . company-complete-common-or-cycle)
         ("<backtab>"  . company-select-previous)
         ("C-n"        . company-select-next)
         ("C-p"        . company-select-previous)))

(use-package company-prescient
  :disabled
  :init (company-prescient-mode 1))

(use-package company-lsp
  :custom (company-lsp-cache-candidates 'auto)
  :config (add-to-list 'company-backends 'company-lsp))

(use-package company-tabnine
  :after company
  :bind (:map company-active-map
              ("M-q" . company-other-backend))
  :config
  (add-to-list 'company-backends 'company-tabnine)
  (setq company-tabnine-always-trigger nil))

(use-package company-emoji
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
              ("M-h" . company-box-doc-manually))
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text          . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method        . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Function      . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Constructor   . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Field         . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0))
          (Variable      . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0))
          (Class         . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2))
          (Interface     . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2))
          (Module        . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2))
          (Property      . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value         . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2))
          (Enum          . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2))
          (Keyword       . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet       . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color         . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File          . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference     . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder        . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember    . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2))
          (Constant      . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
          (Struct        . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2))
          (Event         . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0))
          (Operator      . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template      . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
        ))

(use-package counsel
  :after ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("s-V"     . counsel-yank-pop))
  :config
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n----------\n"
      ))

(use-package counsel-projectile
  :after (projectile counsel)
  :commands counsel-projectile-switch-to-buffer
  :bind (("C-x C-b" . counsel-projectile-switch-to-buffer)
         ("C-o"     . counsel-projectile-find-file)
         ("M-s"     . counsel-projectile-rg)))

(use-package counsel-osx-app
  :if (memq window-system '(mac ns))
  :bind (("s-O" . counsel-osx-app)))

(use-package swiper
  :bind (("C-;" . swiper)
         ("C-:" . swiper-all)))

(use-package ivy-hydra :after ivy)
(use-package ivy
  :defer 1
  :defines (ivy-set-font)
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-count-format "(%d/%d) ")
  (defun ivy-set-font ()
    (interactive)
    (let* ((re "-\\*-\\([a-z0-9/ ]+\\)-\\([a-z]+\\).*" )
           (candidates (--map
                        (cons (s-replace-regexp re "\\1 (\\2)" it) it)
                        (--filter (and (s-contains-p "normal-normal-*" it)
                                       (s-contains-p "-m-0" it))
                                  (x-list-fonts "*" nil (selected-frame))))))
      (ivy-read "Font: " candidates :action (lambda (x) (funcall 'set-frame-font (cdr x))))))

  :bind (:map ivy-minibuffer-map
         ("<backspace>" . delete-backward-char)
         ("C-d"         . delete-forward-char)))

(use-package ivy-prescient
  :init (ivy-prescient-mode 1))

(use-package ivy-posframe
  :if window-system
  :hook (ivy-posframe-mode . (lambda () (set-window-fringes nil 0 0)))
  :after ivy
  :config
  (setq ivy-posframe-border-width 20
        ivy-posframe-min-width 60
        ivy-posframe-width 120
        ivy-posframe-min-height 20
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (bind-keys :map ivy-minibuffer-map
             ("C-'" . ivy-posframe-avy))
  (ivy-posframe-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode))

(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (which-key-mode)
  )

;; LSP servers
;; go - GO111MODULE=on go get golang.org/x/tools/gopls@latest

(use-package lsp-mode
  :hook ((go-mode  . lsp-deferred)
         (js2-mode . lsp-deferred))
  :commands (lsp))

(bind-keys
 ("C-M-/" . hippie-expand))

(provide 'completion)
;;; completion.el ends here
