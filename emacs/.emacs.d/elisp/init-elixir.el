(use-package alchemist
  :ensure t)

(general-define-key
 :states '(normal visual)
 :keymaps 'alchemist-mode-map
 "gd" 'alchemist-goto-definition-at-point)

;; erlang
(use-package erlang
  :ensure t)

(provide 'init-elixir)
