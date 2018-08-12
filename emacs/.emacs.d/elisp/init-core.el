;; interface
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq column-number-mode t)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font "DejaVu Sans Mono")
  (set-face-attribute 'default nil :height 100))

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)

;; highlight matching brackets
(show-paren-mode 1)

;; allow more than 800 KiB cache before starting garbage collection
(setq gc-cons-threshold 50000000)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; allow text filling to end lines on period
(setq sentence-end-double-space nil)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator " • ")

;; emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; dired
(setq dired-listing-switches "-alh")

;; use newline at the end of file
(setq require-final-newline t)

;; 'y' or 'n' without needing to press enter
(defalias 'yes-or-no-p 'y-or-n-p)

;; reload tags file without querying
(setq tags-revert-without-query t)

;; save cursor location in file
(save-place-mode 1)

;; cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'init-core)
