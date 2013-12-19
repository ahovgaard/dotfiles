#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#PS1='[\u@\h \W]\$ '
PS1='\[\e[0;32m\]\u\[\e[m\] \[\e[1;34m\]\w\[\e[m\] \[\e[1;32m\]\$\[\e[m\] \[\e[1;37m\]'

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

# Add "~/bin" to the $PATH shell variable
export PATH="${PATH}:~/bin"

# Add "/opt/mosml/bin" to the $PATH shell variable
export PATH="${PATH}:/opt/mosml/bin"

# Git bash tab completion
source /usr/share/git/completion/git-completion.bash

# Misc useful aliases
alias ls='ls --color=auto -F --group-directories-first'
alias la='ls --color=auto -a'
alias ll='ls --color=auto -lh'
alias grep='grep -n --color=auto'
alias feh='feh --fullscreen --hide-pointer'
alias mosml='rlwrap mosml -P full'
alias tmux='tmux -2'

# Run 'startx' when logging into ttty1 and X isn't alrady running
if [[ $(tty) = /dev/tty1 ]] && [[ -z "$DISPLAY" ]]; then
  exec startx
fi
