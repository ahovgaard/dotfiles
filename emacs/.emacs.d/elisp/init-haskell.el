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

  (general-define-key
   :states '(normal visual)
   :keymaps 'haskell-mode-map
   :prefix "SPC m"

   "if" 'haskell-mode-format-imports  ;; managing imports
   "is" 'haskell-sort-imports
   "ia" 'haskell-align-imports

   "cl" 'haskell-process-load-file    ;; compilation and REPL
   "cc" 'haskell-compile
   "cb" 'haskell-interactive-bring
   "ct" 'haskell-process-do-type
   "ci" 'haskell-process-do-info
   )
  )

;;(define-key interactive-haskell-mode-map
;;  (kbd "M-.") 'haskell-mode-goto-loc)

(setq haskell-tags-on-save t)

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

;; hybrid GHCi/tags go to defintion
;;(define-key haskell-mode-map (kbd "gd") 'haskell-mode-jump-to-def-or-tag)

;; Intero
;;(use-package intero
;;  :ensure t
;;  :diminish haskell-mode
;;  :diminish intero-mode " λ"
;;  :init
;;  (evil-define-key '(insert normal) intero-mode-map
;;    (kbd "M-.") 'intero-goto-definition)
;;  :config
;;  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'init-haskell)
