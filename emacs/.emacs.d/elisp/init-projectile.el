(use-package projectile
  :ensure t
  :commands (helm-projectile-switch-project
             helm-projectile-find-file)

  :init
  (use-package helm-projectile
    :ensure t)

  (setq projectile-sort-order 'recentf
        projectile-switch-project-action 'helm-projectile-find-file
        projectile-enable-caching t
        projectile-cache-file (concat cache-directory
                                      "projectile.cache")
        projectile-known-projects-file (concat cache-directory
                                               "projectile-bookmarks.eld"))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "p"  '(:ignore t :which-key "projectile")
   "pp" 'helm-projectile-switch-project
   "pf" 'helm-projectile-find-file
   "pb" 'helm-projectile-switch-to-buffer
   "pk" 'projectile-kill-buffers)

  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(provide 'init-projectile)
