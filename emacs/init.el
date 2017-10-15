;; ----------------------------------------------------------------------------
;; Package management
;; ----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)

;; auto-install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package.el in not needed at runtime
(eval-when-compile
  (require 'use-package))
(require 'diminish)       ;; if you use :diminish
(require 'bind-key)       ;; if you use any :bind variant


;; ----------------------------------------------------------------------------
;; Modular text editing
;; ----------------------------------------------------------------------------

;; evil-mode
(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init    ;; execute the following before package is loaded
  (setq evil-want-C-u-scroll t)
  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode)
    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key "x" 'helm-M-x))
  :config  ;; execute after package is loaded
  (evil-mode 1)
  (define-key evil-ex-map "b " 'helm-mini)
  (define-key evil-ex-map "e " 'helm-find-files))

; jump over long wrapped lines with j and k
(define-key evil-motion-state-map
  (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
(define-key evil-motion-state-map
  (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)
(define-key evil-operator-state-map
  (kbd "<remap> <evil-next-line>") #'evil-next-line)
(define-key evil-operator-state-map
  (kbd "<remap> <evil-previous-line>") #'evil-previous-line)

;; esc quits
;;(define-key minibuffer-local-map
;;  [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-ns-map
;;  [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-completion-map
;;  [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-must-match-map
;;  [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-isearch-map
;;  [escape] 'minibuffer-keyboard-quit)

;; scrolling other window using vim-like key bindings
;; (defun my-scroll-up-other-window ()
;;   (interactive)
;;   (scroll-other-window '-))
;;(define-key evil-normal-state-map
;;  (kbd "C-S-d") 'scroll-other-window)
;;(define-key evil-normal-state-map
;;  (kbd "C-S-u") 'my-scroll-up-other-window)


;; ----------------------------------------------------------------------------
;; Basic configuration
;; ----------------------------------------------------------------------------

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator " • ")

;; custom variables set by emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; dired
(setq dired-listing-switches "-alh")

;; use newline at the end of file
(setq require-final-newline t)


;; ----------------------------------------------------------------------------
;; General text editing configuration
;; ----------------------------------------------------------------------------

(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)


;; ----------------------------------------------------------------------------
;; Various essential packages
;; ----------------------------------------------------------------------------

;; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :init

  ;; Changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  :config
  (require 'helm)
  (require 'helm-files)
  (require 'helm-config)

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key [escape] 'helm-keyboard-quit helm-map)

  (helm-mode 1)

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  :bind (("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x))
  )

;;(global-set-key (kbd "C-c h g") 'helm-google-suggest)
;;(global-unset-key (kbd "C-c h C-c g"))


;; org-mode
(use-package org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :init
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/index.org"
                               "~/uni/thesis/thesis.org")))

;; magit
(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode  ;; hide from mode-line
  )


;; ----------------------------------------------------------------------------
;; Interface and themes
;; ----------------------------------------------------------------------------

;; set a default font
;;(when (member "DejaVu Sans Mono" (font-family-list))
;;  (set-default-font "DejaVu Sans Mono")
;;  (set-face-attribute 'default nil :height 110))

;; interface
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq column-number-mode t)         ;; display the current column number
(blink-cursor-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; line numbers
;;(global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

;; theme; other decent themes:
;;  - ample-theme
;;  - color-theme-sanityinc-tomorrow
;;  - ample-zen-theme
;;  - gruvbox
;;(use-package ample-theme
;;  :ensure t
;;  :init
;;  (load-theme 'ample))


;; ----------------------------------------------------------------------------
;; Programming languages
;; ----------------------------------------------------------------------------

;; Futhark
(use-package futhark-mode
  :ensure t)

;; Twelf
(setq twelf-root "/opt/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))

;; CUDA
(use-package cuda-mode
  :ensure t)

;; LaTeX
(use-package tex
  :ensure auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;(setq reftex-plug-into-AUCTeX t)

;;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)


;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; spell checking of strings and comments
  (add-hook 'haskell-mode-hook 'flyspell-prog-mode)

  ;; flycheck
  (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; interactive haskell
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (setq haskell-compile-cabal-build-command "stack build --fast"
        haskell-ask-also-kill-buffers nil
        haskell-process-type 'stack-ghci  ;; use stack as build tool
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-interactive-popup-errors nil)

  ;; keybindings
  (define-key haskell-mode-map (kbd "<f8>")
    'haskell-navigate-imports)

  (evil-leader/set-key-for-mode 'haskell-mode    ;; managing imports
    "if" 'haskell-mode-format-imports
    "is" 'haskell-sort-imports
    "ia" 'haskell-align-imports)

  (evil-leader/set-key-for-mode 'haskell-mode    ;; compilation and REPL
    "cl" 'haskell-process-load-or-reload
    "cc" 'haskell-compile
    "cb" 'haskell-interactive-bring
    "ct" 'haskell-process-do-type
    "ci" 'haskell-process-do-info))

;;(define-key interactive-haskell-mode-map
;;  (kbd "M-.") 'haskell-mode-goto-loc)

;;(setq haskell-tags-on-save t)

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

;; hybrid GHCi/tags go to defintion
;;(define-key haskell-mode-map (kbd "gd") 'haskell-mode-jump-to-def-or-tag)
