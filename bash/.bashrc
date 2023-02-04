#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

# Code syntax coloring in less (requires the source-highlight package).
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS=' -R '

# Colored man pages
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
      man "$@"
}

# General environment variables
export EDITOR=vim

# Additions to the $PATH environment variable
export PATH=$PATH:~/bin
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.opam/default/bin

# Git bash tab completion
source /usr/share/git/completion/git-completion.bash

# Misc useful aliases
alias ls='ls --color=auto -F --group-directories-first'
alias la='ls --color=auto -a'
alias ll='ls --color=auto -lh'
alias grep='grep -n --color=auto'
alias feh='feh --fullscreen --hide-pointer'
alias tmux='tmux -2'

if type "nvim" > /dev/null; then
  alias vim='nvim'
fi

# Run 'startx' when logging into ttty1 and X isn't alrady running
if [[ $(tty) = /dev/tty1 ]] && [[ -z "$DISPLAY" ]]; then
  exec startx
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
. "$HOME/.cargo/env"
