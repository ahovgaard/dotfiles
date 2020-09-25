(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (add-hook 'org-mode-hook
            (lambda () (set-fill-column 80)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-src-fontify-natively t)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(setq org-log-done t)
(setq org-agenda-files (list "~/org/notes.org"
                             "~/uni/thesis/thesis.org"))

(add-hook 'org-mode-hook 'auto-fill-mode)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"

 "o"  '(:ignore t :which-key "org")
 "oa" 'org-agenda
 "oc" 'org-capture
 "ol" 'org-insert-link
 "ov" 'org-toggle-inline-images)

(provide 'init-org)
