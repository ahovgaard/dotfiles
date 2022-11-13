;; -*- lexical-binding: t -*-

;; Basic settings
;; ---------------------------------------------------------------------

;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes. Measured in bytes.
(let ((normal-gc-cons-threshold (* 2 1000 1000))
      (init-gc-cons-threshold (* 50 1000 1000)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
	    ;; Make gc pauses faster by decreasing the threshold.
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(defun akh/display-startup-time ()
  "Displays Emacs startup time and garbage collections."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'akh/display-startup-time)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Turn off some unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Enable highlighting of the current line
(hl-line-mode 1)

;; Display line numbers in every buffer
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; Font
(defvar akh/font-family "Fira Code"
  "The default font.")
(defvar akh/font-height 110
  "The default font height.")

(if (member akh/font-family (font-family-list))
    (set-face-attribute
     'default nil :font akh/font-family :height akh/font-height)
  (warn "Font \"%s\" is not available." akh/font))

(setq text-scale-mode-step 1.1)

;; Confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Save cursor location in files
(save-place-mode 1)

;; Package management
;; ---------------------------------------------------------------------

;; Install the straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package with straight.el
(straight-use-package 'use-package)

;; Install packages by default in `use-package` forms,
;; without having to specify `:straight t`
(setq straight-use-package-by-default t)

;; Completion
;; ---------------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)

(use-package embark
  :bind (:map minibuffer-local-completion-map
	      ("C-c C-o" . embark-export)))

(use-package embark-consult)

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; Evil
;; ---------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Key bindings
;; ---------------------------------------------------------------------

(use-package hydra)

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0))

(use-package general
  :config
  (general-create-definer akh/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC"))

(akh/leader-key
  "s"  '(:ignore t :which-key "search")
  "ss" 'consult-line
  "sg" 'consult-git-grep)

(akh/leader-key
  "w"  '(:ignore t :which-key "window")
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wo" 'delete-other-windows
  "w>" 'evil-window-increase-width
  "w<" 'evil-window-decrease-width
  "w+" 'evil-window-increase-height
  "w-" 'evil-window-decrease-height
  "w=" 'balance-windows)

(akh/leader-key
  "f"  '(:ignore t :which-key "file")
  "fs" 'save-buffer)

;; Theme and UI
;; ---------------------------------------------------------------------

;; Themes
(use-package doom-themes
  :init
  (load-theme 'doom-one t))

;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Helpful
;; ---------------------------------------------------------------------

(use-package helpful
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

;; Project interaction
;; ---------------------------------------------------------------------

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(akh/leader-key
  "p"  '(:ignore t :which-key "project")
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pi" 'projectile-invalidate-cache)

;; Version control
;; ---------------------------------------------------------------------

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(akh/leader-key
  "g"  '(:ignore t :which-key "git")
  "gg" 'magit-status)

(use-package forge)

;; Org-mode
;; ---------------------------------------------------------------------

(use-package org
  :straight nil ;; use the built-in version of org-mode
  :config
  ;; characters to use for the ellipsis:
  ;; …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱.
  (setq org-ellipsis " ▼"))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; Languages
;; ---------------------------------------------------------------------

; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;;(XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; Rust
(use-package rustic)

;; Markdown
(use-package markdown-mode)
