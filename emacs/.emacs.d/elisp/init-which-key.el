(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  :diminish which-key-mode)  ;; hide from mode-line

(provide 'init-which-key)
