;; -------------------------------------------------------------------
;; Modular text editing with Evil
;; -------------------------------------------------------------------

(use-package evil
  :ensure t
  :diminish undo-tree-mode

  :init  ;; execute the following before package is loaded
  (setq evil-want-C-u-scroll t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-want-integration nil)

  :config  ;; execute after package is loaded
  (evil-mode 1)

  ;; ugly fix to get vim behavior of C-u in insert mode
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (if (looking-back "^" 0)
          (backward-delete-char 1)
        (if (current-line-empty-p)
            (evil-delete (point-at-bol) (point))
          (evil-delete
           (save-excursion (back-to-indentation) (point))
           (point))))))

  (define-key evil-ex-map "b " 'ivy-switch-buffer)
  (define-key evil-ex-map "e " 'counsel-find-file)

  (define-key evil-normal-state-map "g]" 'helm-etags-select))

; jump over long wrapped lines with j and k
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

;; try to make ansi-term behave better
(evil-set-initial-state 'term-mode 'normal)
;;(evil-define-key 'normal term-raw-map (kbd "C-c") 'term-send-raw)
(evil-define-key 'insert term-raw-map (kbd "C-c") 'term-send-raw)

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

(provide 'init-evil)
