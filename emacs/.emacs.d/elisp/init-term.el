(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/zsh"
        term-buffer-maximum-size 4096))

(defun term-send-up    () (interactive) (term-send-raw-string "\e[A"))
(defun term-send-down  () (interactive) (term-send-raw-string "\e[B"))
(defun term-send-right () (interactive) (term-send-raw-string "\e[C"))
(defun term-send-left  () (interactive) (term-send-raw-string "\e[D"))

(evil-set-initial-state 'term-mode 'normal)
(evil-collection-define-key 'insert 'term-raw-map
  (kbd "C-c") 'term-send-raw
  (kbd "M-p") 'term-send-up
  (kbd "M-n") 'term-send-down)

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
 "t"  '(:ignore t :which-key "terminal")
 "tt" 'multi-term
 "tn" 'multi-term-next
 "tp" 'multi-term-prev)

(provide 'init-term)
