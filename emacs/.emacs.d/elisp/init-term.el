(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/zsh"
        term-buffer-maximum-size 4096))

(evil-set-initial-state 'term-mode 'normal)
(evil-collection-define-key 'insert 'term-raw-map
  (kbd "C-c") 'term-send-raw)

(provide 'init-term)
