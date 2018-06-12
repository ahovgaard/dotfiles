;; Try out a package.
(use-package try
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Predicate that tests if the currect line is empty (ignoring whitespace).
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; Delete the current file.
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Fill and unfill paragraphs with a single key
;; (from http://endlessparentheses.com)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; Folding
(use-package evil-vimish-fold
  :ensure t
  :config
  (evil-vimish-fold-mode 1))

(provide 'init-misc)
