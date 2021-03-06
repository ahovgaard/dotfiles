;; -*- lexical-binding: t -*-

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

;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; custom variables set by emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst cache-directory
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Storage area for persistent files.")

(require 'init-core)
(require 'init-evil)
(require 'init-theme)
(require 'init-misc)
(require 'init-coding)

(require 'init-org)
(require 'init-pdf)
(require 'init-term)

(require 'init-futhark)
(require 'init-twelf)
(require 'init-haskell)
(require 'init-tex)
(require 'init-python)
(require 'init-elixir)
