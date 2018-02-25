(use-package swiper
  :ensure t
  ;; use swiper in place of the default incremental search
  :bind ("C-s" . swiper)
  :config
  (progn
    ;; exit with escape
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key swiper-map [escape] 'minibuffer-keyboard-quit)))

(use-package counsel
  :ensure t)

(provide 'init-swiper)
