;; theme; other decent themes:
;;   ample-theme, color-theme-sanityinc-tomorrow, doom-vibrant
;;   ample-zen-theme, gruvbox, monokai, ujelly, wombat, moe-theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai))

;; cursor color
(set-cursor-color "#ff0000")

(provide 'init-theme)
