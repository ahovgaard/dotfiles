;; libvterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)

  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package multi-vterm :ensure t)

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
 "t"  '(:ignore t :which-key "terminal")
 "tt" 'multi-vterm
 "tn" 'multi-vterm-next
 "tp" 'multi-vterm-prev)

; (use-package multi-vterm
;       :config
;       (add-hook 'vterm-mode-hook
;                       (lambda ()
;                       (setq-local evil-insert-state-cursor 'box)
;                       (evil-insert-state)))
;       (define-key vterm-mode-map [return]                      #'vterm-send-return)
;
;       (setq vterm-keymap-exceptions nil)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
;       (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
;       (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;       (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
;       (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
;       (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
;       (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
;       (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
;       (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

; (when (require 'multi-term nil t)
;   (global-set-key (kbd "<f5>") 'multi-term)
;   (global-set-key (kbd "<C-next>") 'multi-term-next)
;   (global-set-key (kbd "<C-prior>") 'multi-term-prev)
;   (setq multi-term-buffer-name "term"
;         multi-term-program "/bin/zsh"
;         term-buffer-maximum-size 4096))
;
; (defun term-send-up    () (interactive) (term-send-raw-string "\e[A"))
; (defun term-send-down  () (interactive) (term-send-raw-string "\e[B"))
; (defun term-send-right () (interactive) (term-send-raw-string "\e[C"))
; (defun term-send-left  () (interactive) (term-send-raw-string "\e[D"))
;
; (evil-set-initial-state 'term-mode 'normal)
; (evil-collection-define-key 'insert 'term-raw-map
;   (kbd "C-c") 'term-send-raw
;   (kbd "M-p") 'term-send-up
;   (kbd "M-n") 'term-send-down)

(provide 'init-term)
