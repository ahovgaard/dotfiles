# Normally, the path should be set in ~/.zshenv, but Arch Linux sources
# /etc/profile after sourcing ~/.zshenv. To prevent your $PATH being
# overwritten, set it in ~/.zprofile. 
typeset -U path
path=(~/bin ~/.local/bin $path[@])

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
