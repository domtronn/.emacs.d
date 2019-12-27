;;; completion --- Configuration for code completion
;;; Commentary:
;;; Code:


(use-package amx
  :init
  (setq amx-history-length 20
        amx-show-key-bindings t))

(use-package company
  :config
  (setq company-show-numbers t
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match 'never)
  :hook (after-init    . global-company-mode)
  :bind (("<kp-enter>" . company-complete)
         ("M-/"        . company-complete)
         :map company-active-map
         ("<tab>"      . company-complete-common-or-cycle)
         ("<backtab>"  . company-select-previous)
         ("C-n"        . company-select-next)
         ("C-p"        . company-select-previous)))

(use-package company-racer
  :config (add-to-list 'company-backends 'company-racer))

(use-package company-lsp
  :custom (company-lsp-cache-candidates 'auto)
  :config (add-to-list 'company-backends 'company-lsp))

(use-package company-tabnine
  :after company
  :custom (company-tabnine-max-num-results 9)
  :bind (:map company-active-map
              ("M-q" . company-other-backend))
  :config
  (add-to-list 'company-backends 'company-tabnine)
  (setq company-tabnine-always-trigger t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
              ("M-h" . company-box-doc-manually))
  :config
  (defun company-box--common-make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (candidate-i (--find-index (s-equals-p candidate it) company-candidates))
            (candidate-num (format "%s " (if (< candidate-i 9) (1+ candidate-i) " ")))
            (candidate-kind (company-box--get-kind candidate))
            (annotation-str (format "%s [%s]" (or annotation "") candidate-kind))
            (annotation-len (length annotation-str))
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (concat " " (and company-tooltip-align-annotations
                                           (propertize " " 'display `(space :align-to (- right-fringe ,(or annotation-len 0) 1))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (propertize annotation-str 'face 'company-box-annotation))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-num i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ 2 len-c annotation-len)
                                       'company-box--color s-color)
                           line)
      line))
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
        )

  (advice-add 'company-box--make-line :override 'company-box--common-make-line)
  )

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
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))

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
  (defun ivy-set-font (&optional face)
    (interactive)
    (let* ((re "-\\*-\\([a-z0-9/ ]+\\)-\\([a-z]+\\).*" )
           (candidates (--map
                        (cons (s-replace-regexp re "\\1 (\\2)" it) it)
                        (--filter (and (s-contains-p "normal-normal-*" it)
                                       (s-contains-p "-m-0" it))
                                  (x-list-fonts "*" nil (selected-frame))))))
      (ivy-read "Font: " candidates
                :action (lambda (x) (funcall 'set-face-font
                                        (or face 'default)
                                        (cdr x)
                                        (selected-frame))))))

  (defun ivy-set-face-font ()
    (interactive)
    (let ((face (read-face-name "Face: " (face-at-point t))))
      (ivy-set-font face)))

  :bind (:map ivy-minibuffer-map
              ("s-s"         . (lambda () (interactive) (ivy-quit-and-run (rg-project ivy-text "*"))))
              ("s-f"         . (lambda () (interactive) (ivy-quit-and-run (counsel-rg))))
              ("s-o"         . (lambda () (interactive) (ivy-quit-and-run (counsel-projectile))))
              ("s-p"         . (lambda () (interactive) (ivy-quit-and-run (counsel-projectile-switch-project))))
              ("<backspace>" . delete-backward-char)
              ("C-d"         . delete-forward-char)))

(use-package ivy-prescient
  :init (ivy-prescient-mode 1))

(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind ("C-\\" . ivy-yasnippet))

(use-package ivy-posframe
  :if window-system
  :hook ((minibuffer-setup ivy-rich-mode) . (lambda () (set-window-fringes nil 0 0)))
  :after ivy
  :config
  (setq ivy-posframe-border-width 30
        ivy-posframe-min-width 60
        ivy-posframe-width 120
        ivy-posframe-min-height 20
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (bind-keys :map ivy-minibuffer-map
             ("C-'"        . ivy-posframe-avy)
             ("<C-return>" . ivy-read-action))
  (ivy-posframe-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode))

(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

;; LSP servers
;; go - GO111MODULE=on go get golang.org/x/tools/gopls@latest

(use-package lsp-mode
  :hook (((go-mode
           js2-mode
           rjsx-mode
           rust-mode
           rustic-mode) . lsp-deferred))
  :commands (lsp))

(bind-keys
 ("C-M-/" . hippie-expand))

(provide 'completion)
;;; completion.el ends here
