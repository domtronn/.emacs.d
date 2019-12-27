;;; rust.el --- Configuration for programming in rust

;;; Commentary:
;;
;; brew install rustup
;; rustup toolchain add nightly
;; rustup component add rust-src
;; rustup component add rls rust-analysis rust-src
;; cargo +nightly install racer

;;; Code:

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(provide 'rust)

;;; rust.el ends here
