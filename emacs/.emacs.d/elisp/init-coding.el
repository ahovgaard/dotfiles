(use-package flycheck
  :ensure t)

(global-flycheck-mode)

(use-package magit
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"
   "xg" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :ensure t)

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
   "pk" 'projectile-kill-buffers)
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
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package counsel-etags
  :ensure t)
(require 'counsel-etags)

(provide 'init-coding)
