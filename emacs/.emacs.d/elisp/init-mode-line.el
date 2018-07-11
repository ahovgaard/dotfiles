(use-package smart-mode-line
  :commands sml/setup
  :demand t
  :init
  (setq sml/theme 'dark
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))

(provide 'init-mode-line)
