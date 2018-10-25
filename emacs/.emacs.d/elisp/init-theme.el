;;; init-theme.el -*- lexical-binding: t; -*-

;; theme; other decent themes:
;;   ample-theme, color-theme-sanityinc-tomorrow, doom-vibrant
;;   ample-zen-theme, gruvbox, monokai, ujelly, wombat, moe-theme
(use-package leuven-theme
  :ensure t
  :defer t)
(use-package monokai-theme
  :ensure t
  :defer t)

(setq akh/themes '(monokai leuven))
(setq akh/themes-index 0)

(defun akh/cycle-theme ()
  "Cycle through themes."
  (interactive)
  (disable-theme (nth akh/themes-index akh/themes))
  (setq akh/themes-index (% (1+ akh/themes-index) (length akh/themes)))
  (load-theme (nth akh/themes-index akh/themes) t))

(load-theme 'monokai)

(use-package smart-mode-line
  :ensure t
  :commands sml/setup
  :demand t
  :init
  (setq sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))

(provide 'init-theme)
