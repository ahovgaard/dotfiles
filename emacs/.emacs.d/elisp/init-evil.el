;; -------------------------------------------------------------------
;; Modular text editing with Evil
;; -------------------------------------------------------------------

(use-package evil
  :ensure t
  :diminish undo-tree-mode

  :init  ;; execute the following before package is loaded
  (setq evil-want-C-u-scroll t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-want-keybinding nil)  ;; required by evil-colleciton

  :config  ;; execute after package is loaded
  (evil-mode 1)

  (define-key evil-ex-map "b " 'ivy-switch-buffer)
  (define-key evil-ex-map "e " 'counsel-find-file))

;;  ;; ugly fix to get vim behavior of C-u in insert mode
;;  (define-key evil-insert-state-map (kbd "C-u")
;;    (lambda ()
;;      (interactive)
;;      (if (looking-back "^" 0)
;;          (backward-delete-char 1)
;;        (if (current-line-empty-p)
;;            (evil-delete (point-at-bol) (point))
;;          (evil-delete
;;           (save-excursion (back-to-indentation) (point))
;;           (point))))))

;; jump over long wrapped lines with j and k
(define-key evil-motion-state-map
  (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
(define-key evil-motion-state-map
  (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)
(define-key evil-operator-state-map
  (kbd "<remap> <evil-next-line>") #'evil-next-line)
(define-key evil-operator-state-map
  (kbd "<remap> <evil-previous-line>") #'evil-previous-line)

;; treat underscore as part of the word
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
;; alternative approach which modifies the syntax table to treat _ as
;; a word constituent rather than a symbol constituent, for some mode:
;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; make escape exit
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package counsel
  :ensure t
  :diminish counsel-mode
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
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit)
  ;; use swiper in place of the default incremental search
  (define-key evil-normal-state-map (kbd "C-s") 'swiper))

(use-package general
  :ensure t
  :after evil
  :config

  ;; global keybindings
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "w"  '(:ignore t :which-key "windows")
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

   "b"  '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "bB" 'ibuffer
   "bd" 'kill-current-buffer

   "f"  '(:ignore t :which-key "files")
   "fd" 'counsel-git

   "/"  'counsel-ag
   )
  (general-define-key
   :states '(normal visual)
   :prefix ","
   "x" 'counsel-M-x
   "f" 'counsel-find-file
   "b" 'ivy-switch-buffer
   )
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode 1))

(provide 'init-evil)
