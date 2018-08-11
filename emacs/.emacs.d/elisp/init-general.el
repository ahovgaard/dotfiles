(use-package general
  :ensure t)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"

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

 "bb" 'ivy-switch-buffer
 "bk" 'kill-buffer

 "f"  '(:ignore t :which-key "Files")
 "fd" '(counsel-git :which-key "counsel-git: find in git repo")
 )

(provide 'init-general)
