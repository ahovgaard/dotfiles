(use-package flycheck
  :ensure t
  :init
  (add-hook 'python-mode-hook 'flycheck-mode))

(provide 'init-syntax-checking)
