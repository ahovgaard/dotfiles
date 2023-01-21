;;; -*- lexical-binding: t -*-

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

;; Display emacs startup time and number of garbage collections.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time "%.2f seconds") gcs-done)))

;; Show an empty scratch buffer after startup
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
  (warn "Font \"%s\" is not available." akh/font-family))

(setq text-scale-mode-step 1.1)

;; Confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Save cursor location in files
(save-place-mode 1)

;; Enable Recentf mode to keep track of recently opened files.
(recentf-mode 1)

;; Automatically add a newline at the end of a file when a file is
;; saved. The POSIX standard defines a "line" as ending in a newline
;; character.
(setq require-final-newline t)

;; Indent using spaces, instead of tabs, by default.
(setq-default indent-tabs-mode nil)

;; Cleanup whitespace on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Scroll one line at a time.
(setq scroll-conservatively 1000)

;; Package management
;; ---------------------------------------------------------------------

;; Use the `straight.el` package manager instead of the built-in
;; `package.el`, for more a reproducible configuration. `package.el`
;; is disabled in the early init file.
;;
;; https://github.com/radian-software/straight.el
;;
;; - Upgrade all packages: `straight-pull-all`
;; - Crate/update the lockfile: `straight-freeze-versions`.
;; - Roll back to versions pinned in the lockfile: `straight-thaw-versions`.
;;
;; Lockfile under version control: `.emacs.d/straight/versions/default.el`

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
  (savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)

(use-package embark
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

(use-package embark-consult)

;; Enhanced completion at point with Corfu and Cape.
;; https://github.com/minad/corfu

(use-package cape)

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history corfu-popupinfo))
  ;; Optional customizations
  :custom
  (corfu-cycle nil)                 ;; Disable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 2)        ;; Use scroll margin

  ;; (setq corfu-min-width 70)
  ;; (setq corfu-max-width corfu-min-width)  ;; Always have the same width

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :config
  ;; Enable completion in the minibuffer, e.g., for commands like
  ;; `M-:' (`eval-expression') or `M-!' (`shell-command'), when other
  ;; completion UI is not active.
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto t)         ;; Enable auto completion
      (setq-local corfu-echo-delay nil  ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (setq corfu-auto-prefix 2)
  (setq corfu-popupinfo-delay 0)
  ;; (set-face-attribute 'corfu-current nil :inherit 'highlight :background nil :foreground nil))
  )

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; LSP
;; ---------------------------------------------------------------------

;; Configure Corfu and `lsp-mode` to work together.
;; https://github.com/minad/corfu/wiki

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)  ;; Use Corfu for LSP completion

  :init
  (setq lsp-keymap-prefix "C-c l")

  (defun akh/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun akh/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (elixir-mode . lsp)
         ;;(XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . akh/lsp-mode-setup-completion))

  :config
  (setq lsp-headerline-breadcrumb-enable nil))

;;(use-package lsp-ui
;;  :after lsp
;;  :commands lsp-ui-mode)

;; Evil
;; ---------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Undo/redo
;; ---------------------------------------------------------------------

;; Highlights undos by flashing to-be-deleted text before deleting.
;; https://github.com/casouri/undo-hl
(use-package undo-hl
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl")
  :commands undo-hl-mode
  :init
  (add-hook 'prog-mode-hook #'undo-hl-mode)
  (add-hook 'text-mode-hook #'undo-hl-mode))

;; Visual undo. Displays the undo history as a tree.
;; https://github.com/casouri/vundo
(use-package vundo
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

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
  ;; Create a general.el definer macro using "SPC" as loader key.
  (general-create-definer akh/leader-key
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")

  ;; Create a general.el definer macro using "," as loader key.
  (general-create-definer akh/local-leader-key
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ","))

(akh/leader-key
  "." 'find-file
  "," 'akh/switch-project-buffer)

(akh/local-leader-key
  "x" 'execute-extended-command
  "f" 'find-file
  "b" 'akh/switch-project-buffer
  "B" 'switch-to-buffer)

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
  "fs" 'save-buffer
  "fr" 'consult-recent-file)

(akh/leader-key
  "b"  '(:ignore t :which-key "buffer")
  "bd" 'kill-current-buffer
  "bb" 'akh/switch-project-buffer
  "bB" 'switch-to-buffer)

(akh/leader-key
  "t"  '(:ignore t :which-key "toggle")
  "tl" 'display-line-numbers-mode
  "tw" 'toggle-truncate-lines)

(akh/leader-key
  "c" '(:ignore t :which-key "code")
  "cf" 'lsp-format-buffer
  )

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
  ;; Find file after switching projects.
  ;; Another good option is `projectile-dired'.
  (setq projectile-switch-project-action #'projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(defun akh/switch-project-buffer ()
  "Switch to a project buffer if in a project, otherwise switch to any buffer."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-switch-to-buffer)
    (call-interactively 'switch-to-buffer)))

(akh/leader-key
  "p"  '(:ignore t :which-key "project")
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pb" 'projectile-switch-to-buffer
  "pi" 'projectile-invalidate-cache)

;; Version control
;; ---------------------------------------------------------------------

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(akh/leader-key
  "g"  '(:ignore t :which-key "git")
  "gg" 'magit-status
  "gb" 'magit-blame-addition)

(use-package forge)

;; Org-mode
;; ---------------------------------------------------------------------

(use-package org
  :straight (:type built-in)  ;; use the built-in version of org-mode
  :config
  ;; characters to use for the ellipsis:
  ;; …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱.
  (setq org-ellipsis " ▼"))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; Terminal
;; ---------------------------------------------------------------------

;; emacs-libvterm
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :init
  ;; Disble line numbers in the terminal buffer.
  (add-hook 'vterm-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert))

;; Dired
;; ---------------------------------------------------------------------

(use-package dired
  :straight (:type built-in)
  :init
  (setq dired-listing-switches "-alh"))

;; Languages
;; ---------------------------------------------------------------------

;; Rust
(use-package rustic)

;; Markdown
(use-package markdown-mode)

;; Elixir
(use-package elixir-mode
  :config
  (general-define-key
   :keymaps 'elixir-mode-map
   :prefix "SPC"
   "cf" 'elixir-format))
