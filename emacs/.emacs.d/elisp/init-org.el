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

(provide 'init-org)
