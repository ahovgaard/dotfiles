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

;; Increase the maximum number of bytes to read from a subprocess in a
;; single chunk. Based on `lsp-mode' performance recommandations:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))  ;; 1 MiB

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
(global-hl-line-mode 1)

;; Display the column number in the mode line
(column-number-mode 1)

;; Display line numbers in every buffer (enables
;; `Display-Line-Numbers' mode in all buffers).
;; Disabled in certain mode hooks where not appropriate.
(global-display-line-numbers-mode 1)

;; Set minimum width for line number display to 3 to avoid the gutter
;; changing size when scrolling past line 100.
(setq-default display-line-numbers-width 3)

;; Don't pop up the *Warnings* buffer on warnings and errors from
;; async native compilation.
(setq native-comp-async-report-warnings-errors 'silent)

;; Font
(set-frame-font "DejaVu Sans Mono 11" nil t)

;; Text scale step when using C-x C-{+,-,0}.
(setq text-scale-mode-step 1.1)

;; Confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Save cursor location in files
(save-place-mode 1)

;; Enable Recentf mode to keep track of recently opened files.
(recentf-mode 1)
(setq recentf-max-saved-items 200)  ; default is 20

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

;; Create backups for all files in the same directory (default is to
;; create the backup in the same directory as the original file).
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))


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


;; Common packages
;; ---------------------------------------------------------------------

;; dash.el - modern list API for Emacs.
(use-package dash)


;; Theme and UI
;; ---------------------------------------------------------------------

;; Themes
(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (setq custom-safe-themes t))

;; Required by `doom-modeline` to display icons.
;; Run `M-x nerd-icons-install-fonts` to install the necessary fonts.
(use-package nerd-icons)

;; Mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1))


;; Evil
;; ---------------------------------------------------------------------

(use-package evil
  :init
  ;; Required by evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; C-u should scroll up (like Vim).
  (setq evil-want-C-u-scroll t)
  ;; C-i should jump forward in the jump list (like Vim).
  (setq evil-want-C-i-jump t)
  ;; Make * and # searches use symbols instead of words.
  (setq evil-symbol-word-search t)
  ;; When pasting in visual state, don't add the replaced text to the kill ring.
  (setq evil-kill-on-visual-paste nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :init
  ;; Suppress message on startup, "Setting ‘forge-add-default-bindings’ to
  ;; nil in ‘evil-collection-forge-setup’", by setting it to `nil` here.
  (setq forge-add-default-bindings nil)
  :config
  (evil-collection-init))

;; Support searching with * and # from visual selection.
;; https://github.com/bling/evil-visualstar
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))


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


;; Helpful
;; ---------------------------------------------------------------------

(use-package helpful
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))


;; Execution paths
;; ---------------------------------------------------------------------

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Key binding utilities: General, which-key, hydra
;; ---------------------------------------------------------------------

(use-package hydra)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0))

(use-package general
  :config
  ;; Create a general.el definer macro using "SPC" as leader key.
  (general-create-definer akh/leader-key
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; Create a general.el definer macro using "," as leader key.
  (general-create-definer akh/local-leader-key
    :states '(normal visual)
    :keymaps 'override
    :prefix ","))


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
  (setq corfu-cycle nil)                  ;; Disable cycling for `corfu-next/previous'
  (setq corfu-auto t)                     ;; Enable auto completion
  ;; (setq corfu-separator ?\s)           ;; Orderless field separator
  ;; (setq corfu-quit-at-boundary nil)    ;; Never quit at completion boundary
  ;; (setq corfu-quit-no-match nil)       ;; Never quit, even if there is no match
  ;; (setq corfu-preview-current nil)     ;; Disable current candidate preview
  ;; (setq corfu-preselect 'prompt)       ;; Preselect the prompt
  ;; (setq corfu-on-exact-match nil)      ;; Configure handling of exact matches
  (setq corfu-scroll-margin 2)            ;; Use scroll margin

  (setq corfu-min-width 60)
  (setq corfu-max-width corfu-min-width)  ;; Always have the same width

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


;; Comments
;; ---------------------------------------------------------------------

(use-package evil-nerd-commenter
  :config
  (evil-global-set-key 'normal (kbd "g c") #'evilnc-comment-operator)
  (evil-global-set-key 'visual (kbd "g c") #'evilnc-comment-operator))


;; Key bindings
;; ---------------------------------------------------------------------

(akh/leader-key
  "." 'find-file
  "," 'switch-to-buffer
  "<SPC>" 'projectile-find-file)

(akh/local-leader-key
  "x" 'execute-extended-command
  "f" 'find-file
  "b" 'switch-to-buffer
  "B" 'switch-to-buffer)

(akh/leader-key
  "s"  '(:ignore t :which-key "search")
  "ss" 'consult-line
  "sg" 'consult-git-grep
  "sd" '(consult-ripgrep :which-key "consult-ripgrep project")
  "sD" '((lambda () (interactive) (consult-ripgrep t)) :which-key "consult-ripgrep directory"))

(akh/leader-key
  "w"  '(:ignore t :which-key "window")
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wH" 'evil-window-move-far-left
  "wJ" 'evil-window-move-very-bottom
  "wK" 'evil-window-move-very-top
  "wL" 'evil-window-move-far-right
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
  "bb" 'switch-to-buffer
  "bB" 'switch-to-buffer)

(akh/leader-key
  "t"  '(:ignore t :which-key "toggle")
  "tl" 'display-line-numbers-mode
  "tw" 'toggle-truncate-lines)

(akh/leader-key
  "c" '(:ignore t :which-key "code")
  "cf" 'lsp-format-buffer)

(akh/leader-key
  "h"  '(:ignore t :which-key "help")
  "ht" 'load-theme)

(akh/leader-key
 "o"  '(:ignore t :which-key "open")
 "oT" '(akh/vterm-here :which-key "Open terminal")
 "ot" '(vterm-toggle :which-key "Toggle terminal")
 "ou" 'vundo)


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

(akh/leader-key
  "p"  '(:ignore t :which-key "project")
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pb" 'projectile-switch-to-buffer
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "pd" 'projectile-remove-known-project)


;; Version control
;; ---------------------------------------------------------------------

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  ;; NOTE: This might not perform well with many open buffers, need to
  ;; evaluate.
  (defun akh/magit-update-vc ()
    "Update vc in all verson-controlled buffers when magit refreshes."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (vc-refresh-state))))
  (add-hook 'magit-post-refresh-hook #'akh/magit-update-vc)
  (transient-append-suffix 'magit-fetch "-t"
    '("-f" "Force" "--force"))

  (add-hook 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state)))))

(akh/leader-key
  "g" '(:ignore t :which-key "git")
  "gg" 'magit-status
  "gb" 'magit-blame-addition)

(use-package forge)

;; Git gutter indicators
;; https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  ;; Default is 0, meaning update indicators on saving the file.
  ;; (setq git-gutter:update-interval 0.02)
  )

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


;; Org-mode
;; ---------------------------------------------------------------------

(use-package org
  :straight (:type built-in)  ;; use the built-in version of org-mode
  :config
  ;; characters to use for the ellipsis:
  ;; …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱.
  (setq org-ellipsis " ▼")
  (setq org-startup-indented t)
  (setq org-plantuml-exec-mode 'plantuml)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t))))

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
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-y") #'evil-collection-vterm-paste-after))

(use-package vterm-toggle)

(defun akh/vterm-here ()
  "Open a terminal buffer in the current window."
  (interactive)
  (vterm vterm-buffer-name))


;; Dired
;; ---------------------------------------------------------------------

(use-package dired
  :straight (:type built-in)
  :init
  (setq dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook
            (lambda () (display-line-numbers-mode -1))))


;; Miscellaneous
;; ---------------------------------------------------------------------

;; Highlight TODO and similar keywords in comments and strings.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Colorize color names in buffers, e.g., #AAFF77, MidnightBlue.
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; wgrep.el - Writable grep buffer and apply the changes to files
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep)


;; Languages
;; ---------------------------------------------------------------------

;; Rust
(use-package rustic)

;; Markdown
(use-package markdown-mode)

;; Elixir
(use-package elixir-mode
  :config
  (setq lsp-elixir-suggest-specs nil)

  (general-define-key
   :states '(normal visual)
   :keymaps 'elixir-mode-map
   :prefix "SPC"
   "mf" 'elixir-format))

;; Protocol Buffers (protobuf)
(use-package protobuf-mode)

;; YAML
(use-package yaml-mode)

;; Dockerfile
(use-package dockerfile-mode)

;; Kubernetes
(use-package k8s-mode)

;; PlantUML
;; https://plantuml.com/emacs
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-plantuml.html
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'executable))

;; Nix
(use-package nix-mode)

;; Go
(use-package go-mode)
