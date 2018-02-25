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

(provide 'init-projectile)
