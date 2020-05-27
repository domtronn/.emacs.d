(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(company-lsp-cache-candidates (quote auto))
 '(company-tabnine-max-num-results 9 t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(create-lockfiles nil)
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes
   (quote
    ("56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "a4b9eeeabde73db909e6b080baf29d629507b44276e17c0c411ed5431faf87dd" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" default)))
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(flycheck-title-mode t)
 '(fringe-mode 0 nil (fringe))
 '(indent-tabs-mode nil)
 '(insert-directory-program "gls" t)
 '(js-import-quote "'" t)
 '(kaolin-themes-underline-wave nil)
 '(make-backup-files nil)
 '(ns-function-modifier (quote hyper))
 '(package-selected-packages
   (quote
    (nodejs-repl ivy-posframe ivy-purpose swiper expand-region ivy scheme-mode geiser-mode geiser markdown-toc grip-mode js-doc kubernetes eslintd-fix eslint-fix web-mode css-eldoc scss-mode haml-mode rubocopfmt rvm enh-ruby-mode dired-rainbow diredfl dired-quick-sort major-mode-hydra popwin helpful wgrep google-this howdoyou undo-tree which-key visual-regexp js-import rust-mode flycheck-rust company-racer racer company-go flycheck-clojure cider ivy-yasnippet js-react-redux-yasnippets react-snippets js-react-redux-snippets go-snippets yasnippet-snippets yasnippet go-rename go-impl go-tag git-gutter diff-hl diff-hl-mode solaire-moden auto-package-update sudo-edit auto-highlight-symbol auto-highliht-symbol company-tabnine auto-compile doom-themes restart-emacs link-hint which-key-posframe multi-line multiple-cursors goto-chg rg git-link git-timemachine magit-todos git-messenger browse-at-remote duplicate-thing whole-line-or-region shackle symbol-overlay smart-newline eros highlight-symbol company-emoji comapny-emoji spu forge dumb-jump mwim avy-flycheck avy-jump tide visual-regexp-steroids go-fill-struct electric-operator embrace auto-rename-tag rjsx-mode js2-mode json-mode counsel-projectile smartparens-config smartparens move-text quickrun smart-hungry-delete format-all whitespace-cleanup-mode winum uniquify lsp-ui flycheck-title flycheck-inline flycheck company-lsp company-box company-posframe company-quickhelp counsel-osx-app amx ivy-rich ivy-prescient ivy-posframe-font company-prescient treemacs-projectile treemacs-magit treemacs magit projectile ivy-hydra all-the-icons highlight-parentheses highlight-quoted highlight-defined go-eldoc go-mode lsp-mode counsel smex company exec-path-from-shell dashboard esup use-package)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eslintd-fix-executable . "standard")
     (quickrun-option-cmd-alist
      (:command . "env-cmd")
      (:exec "%c node %s")
      (:remove "%n"))
     (geiser-scheme-implementation quote mit)
     (flycheck-eslintrc . ".eslintrc"))))
 '(scroll-bar-mode nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tool-bar-style (quote text-image-horiz)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(symbol-overlay-default-face ((t (:inherit (region bold))))))

(provide 'custom)
