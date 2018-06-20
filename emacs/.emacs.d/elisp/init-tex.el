(use-package tex
  :ensure auctex
  :config
  (setq font-latex-fontify-script nil)
  (setq TeX-save-query nil)

  (add-hook 'LaTeX-mode-hook
            (lambda () (set-fill-column 80))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"

 "mc"  'TeX-command-master
 "mr"  '(:ignore t :which-key "RefTeX")
 "mrc" 'reftex-citation
 "mrg" 'reftex-grep-document)

;; use zathura to view pdf documents
(add-to-list 'TeX-view-program-list '("zathura" "zathura %o"))
(setq TeX-view-program-selection '((output-pdf "zathura")))

(provide 'init-tex)
