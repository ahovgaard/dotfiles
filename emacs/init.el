(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; auto-install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package.el in not needed at runtime
(eval-when-compile
  (require 'use-package))
(require 'diminish)       ;; if you use :diminish
(require 'bind-key)       ;; if you use any :bind variant

(server-start)

;; load files in 'elisp' directory
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-core)
(require 'init-evil)
(require 'init-powerline)
(require 'init-theme)
(require 'init-syntax-checking)
(require 'init-version-control)



;; ----------------------------------------------------------------------------
;; Various essential packages
;; ----------------------------------------------------------------------------


;; try out a package
(use-package try
  :ensure t)

;; avy: jumping to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

;;; helm
(use-package helm
  :ensure t
  :diminish helm-mode

  :config
  (require 'helm)
  (require 'helm-files)
  (require 'helm-config)

  ;; changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key [escape] 'helm-keyboard-quit helm-map)

  (helm-mode 1)

  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (global-unset-key (kbd "C-c h C-c g"))

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  (setq helm-split-window-in-side-p t)

  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)))

;; org-mode
(use-package org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :init
  ;; org-capture
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (define-key global-map "\C-cc" 'org-capture)

  (setq org-src-fontify-natively t)
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/notes.org"
                               "~/uni/thesis/thesis.org")))


;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  :diminish which-key-mode)  ;; hide from mode-line

;; company-mode: complete anything
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.5)  ;; default is 0.5
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; swiper
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



(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package hydra
  :ensure t)

(defhydra hydra-projectile
  (:color teal :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

  ^Find File^        ^Search/Tags^        ^Buffers^       ^Cache^                    ^Project^
  ^---------^        ^-----------^        ^-------^       ^-----^                    ^-------^
  _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
  _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd    _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
  _r_: recent file   ^ ^                  ^ ^             _z_: cache current
  _d_: dir
"
  ("a"   helm-projectile-ag)
  ("b"   helm-projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("f"   projectile-find-file)
  ("F"   projectile-find-file-dwim)
  ("C-f" projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("p"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("q"   nil "cancel" :color blue)
  ("<escape>" keyboard-escape-quit "" :exit t))


;; ----------------------------------------------------------------------------
;; Programming languages
;; ----------------------------------------------------------------------------

;; Futhark
(use-package futhark-mode
  :ensure t)

;; Twelf
(setq twelf-root "/opt/twelf/")
(load (concat twelf-root "emacs/twelf-init.el"))
;; (twelf-font-create-face 'twelf-font-fvar-face 'default "SpringGreen")

;; CUDA
(use-package cuda-mode
  :ensure t)

;; LaTeX
(use-package tex
  :ensure auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)


;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; spell checking of strings and comments
  ;;(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

  ;; flycheck
  ;; (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; interactive haskell
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (setq haskell-compile-cabal-build-command "stack build --fast"
        haskell-ask-also-kill-buffers nil
        haskell-process-type 'stack-ghci  ;; use stack as build tool
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-interactive-popup-errors nil)

  ;; keybindings
  (define-key haskell-mode-map (kbd "<f8>")
    'haskell-navigate-imports)

  (evil-leader/set-key-for-mode 'haskell-mode    ;; managing imports
    "if" 'haskell-mode-format-imports
    "is" 'haskell-sort-imports
    "ia" 'haskell-align-imports)

  (evil-leader/set-key-for-mode 'haskell-mode    ;; compilation and REPL
    "cl" 'haskell-process-load-file
    "cc" 'haskell-compile
    "cb" 'haskell-interactive-bring
    "ct" 'haskell-process-do-type
    "ci" 'haskell-process-do-info))

;;(define-key interactive-haskell-mode-map
;;  (kbd "M-.") 'haskell-mode-goto-loc)

(setq haskell-tags-on-save t)

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

;; hybrid GHCi/tags go to defintion
;;(define-key haskell-mode-map (kbd "gd") 'haskell-mode-jump-to-def-or-tag)


;;;; Intero
;;(use-package intero
;;  :ensure t
;;  :diminish haskell-mode
;;  :diminish intero-mode " λ"
;;  :init
;;  (evil-define-key '(insert normal) intero-mode-map
;;    (kbd "M-.") 'intero-goto-definition)
;;  :config
;;  (add-hook 'haskell-mode-hook 'intero-mode))


;; Markdown
(use-package markdown-mode
  :ensure t)

;; Python
(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))


;; notmuch
(autoload 'notmuch "notmuch" "notmuch mail" t)

(define-key notmuch-show-mode-map "j" 'next-line)
(define-key notmuch-show-mode-map "k" 'previous-line)
(define-key notmuch-search-mode-map "j" 'next-line)
(define-key notmuch-search-mode-map "k" 'previous-line)


;; ----------------------------------------------------------------------------
;; Miscellaneous functions
;; ----------------------------------------------------------------------------

;; Predicate that tests if the currect line is empty (ignoring whitespace).
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))
