(use-package magit
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package evil-magit
  :ensure t)

(provide 'init-version-control)
