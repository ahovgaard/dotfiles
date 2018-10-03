(use-package elixir-mode
  :ensure t)

(general-define-key
 :states '(normal visual)
 :keymaps 'elixir-mode-map
 "g]" 'counsel-etags-find-tag-at-point)

; (eval-after-load 'flycheck
;   '(flycheck-credo-setup))
; (add-hook 'elixir-mode-hook 'flycheck-mode)
;
; (setq flycheck-elixir-credo-strict t)

(use-package erlang
  :ensure t)

(add-hook 'elixir-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'erlang-mode-hook (lambda () (setq truncate-lines t)))

(provide 'init-elixir)
