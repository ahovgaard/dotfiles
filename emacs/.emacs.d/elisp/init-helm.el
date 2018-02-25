(use-package helm
  :ensure t
  :diminish helm-mode

  :config
  (require 'helm)
  (require 'helm-files)
  (require 'helm-config)

  ;; changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key [escape] 'helm-keyboard-quit helm-map)

  (helm-mode 1)

  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (global-unset-key (kbd "C-c h C-c g"))

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  (setq helm-split-window-in-side-p t)

  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)))

(provide 'init-helm)
