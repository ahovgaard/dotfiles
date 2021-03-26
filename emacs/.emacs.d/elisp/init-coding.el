;;; init-coding -*- lexical-binding: t; -*-

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :init
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"
   "xg" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-enable-caching t
        projectile-cache-file (concat cache-directory "projectile.cache")
        projectile-known-projects-file (concat cache-directory "projectile-bookmarks.eld"))
  ;; (define-key evil-normal-state-map (kbd "SPC p") 'projectile-command-map)
  (projectile-mode +1)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "p"  '(:ignore t :which-key "projectile")
   "pp" 'projectile-switch-project
   "pf" 'projectile-find-file
   "pb" 'projectile-switch-to-buffer
   "pk" 'projectile-kill-buffers
   "pr" 'projectile-replace)
  )

;; company-mode: complete anything
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.5)  ;; default is 0.5
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (push 'company-lsp company-backends))

(use-package company-lsp
  :ensure t)

(use-package counsel-etags
  :ensure t)
(require 'counsel-etags)

(use-package docker
  :ensure t
  :commands docker
  :init
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"
   "xd" 'docker))

(use-package yaml-mode
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides
  :init
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'groovy-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package groovy-mode
  :ensure t)

;; Proof General and Coq
(setq coq-compile-before-require t)

;; Kubernetes
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(setq js-indent-level 2)


(use-package yasnippet
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :ensure t)

;;; LSP mode

; (use-package lsp-mode
;   :hook (elixir-mode . lsp)
;   :commands lsp
;   :config
;   (setq lsp-log-io t)
;   (general-define-key
;    :states '(normal visual)
;    :keymaps 'override
;    :prefix "SPC"
;    "l"  '(:ignore t :which-key "LSP")
;    "lh" 'lsp-describe-thing-at-point
;    "ld" 'lsp-find-definition)
;   )
;
; ;; ;; optionally
; (use-package lsp-ui :commands lsp-ui-mode)
; (use-package company-lsp :commands company-lsp)
; (use-package lsp-ivy :commands company-lsp)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(provide 'init-coding)
