(setq twelf-root "/opt/twelf/")
(if (file-directory-p twelf-root)
    (load (concat twelf-root "emacs/twelf-init.el")))
;; (twelf-font-create-face 'twelf-font-fvar-face 'default "SpringGreen")

(provide 'init-twelf)
