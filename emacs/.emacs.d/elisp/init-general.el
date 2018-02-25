(use-package general
  :ensure t
  :config
  ;; named prefix key
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "/"  '(counsel-ag :wich-key "ag")

   ;; keybindings
   "wh" 'evil-window-left
   "wl" 'evil-window-right
   "wj" 'evil-window-down
   "wk" 'evil-window-up

   "ws" 'evil-window-split
   "wv" 'evil-window-vsplit

   "wo" 'delete-other-windows

   "wH" 'evil-window-move-far-left
   "wL" 'evil-window-move-far-right
   "wJ" 'evil-window-move-very-bottom
   "wK" 'evil-window-move-very-top

   "f"  '(:ignore t :which-key "Files")
   "fd" '(counsel-git :which-key "find in git repo")

   "p" 'hydra-projectile/body
   )
  )

(provide 'init-general)
