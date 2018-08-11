;; ivy, counsel, and swiper

(use-package counsel
  :ensure t
  ;; use swiper in place of the default incremental search
  :bind ("C-s" . swiper)
  :config
  (counsel-mode 1)
  ;; add `recentf-mode` and bookmarks to `ivy-switch-buffer`
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; make the prompt line selectable
  (setq ivy-use-selectable-prompt t)
  ;; exit with escape
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit))

(general-define-key
 :states '(normal visual)
 :prefix ","
 "x" 'counsel-M-x
 "f" 'counsel-find-file
 "b" 'ivy-switch-buffer)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "/"  '(counsel-ag :wich-key "ag"))

(provide 'init-ivy)
