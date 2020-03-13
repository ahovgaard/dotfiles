# Normally, the path should be set in ~/.zshenv, but Arch Linux sources
# /etc/profile after sourcing ~/.zshenv. To prevent your $PATH being
# overwritten, set it in ~/.zprofile.
typeset -U path
path=(~/bin ~/.local/bin ~/repos/elixir-ls/release ~/.mix/escripts $path[@])

if [[ -v INSIDE_EMACS ]]; then
  export EDITOR="emacsclient"
else
  # Regular shell
  export EDITOR="nvim"
fi

# Misc env vars
export KUBECTX_IGNORE_FZF=1

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
