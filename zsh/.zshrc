source ~/.zsh/setopt.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/history.zsh

#
# zsh configuration
#

#autoload -Uz compinit promptinit
autoload -Uz compinit
compinit
# promptinit


# prompt
#prompt walters

# autocompletion of command line switches for aliases
setopt COMPLETE_ALIASES

# aliases
alias ls='ls --color=auto'
alias l='ls -lh'
alias la='ls -lah'
alias ec='emacsclient -nc'
alias vim='nvim'

if type "nvim" > /dev/null; then
  alias vim='nvim'
fi

# extended glob syntax
setopt extendedglob


# The following lines were added by compinstall
zstyle :compinstall filename '/home/akh/.zshrc'
# End of lines added by compinstall


#------------------------------
# Prompt
#------------------------------
autoload -U colors zsh/terminfo
colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*' formats "%{${fg[cyan]}%}[%{${fg[green]}%}%s%{${fg[cyan]}%}][%{${fg[blue]}%}%r/%S%%{${fg[cyan]}%}][%{${fg[blue]}%}%b%{${fg[yellow]}%}%m%u%c%{${fg[cyan]}%}]%{$reset_color%}"

setprompt() {
  setopt prompt_subst

  if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then
    p_host='%F{yellow}%M%f'
  else
    p_host='%F{green}%M%f'
  fi

  PS1=${(j::Q)${(Z:Cn:):-$'
    %F{cyan}[%f
    %(!.%F{red}%n%f.%F{green}%n%f)
    %F{cyan}@%f
    ${p_host}
    %F{cyan}][%f
    %F{blue}%~%f
    %F{cyan}]%f
    %(!.%F{red}%#%f.%F{green}%#%f)
    " "
  '}}

  PS2=$'%_>'
  RPROMPT=$'${vcs_info_msg_0_}'
}
setprompt

#------------------------------
# Other
#------------------------------

# Exercism.io completion
if [ -f ~/.config/exercism/exercism_completion.zsh ]; then
  . ~/.config/exercism/exercism_completion.zsh
fi

export ERL_AFLAGS="-kernel shell_history enabled"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Fix for urxvt lines cut off on resize
# https://bugs.launchpad.net/ubuntu/+source/rxvt-unicode/+bug/677425
# https://superuser.com/questions/442589/xmonad-urxvt-issue-text-disappears-after-resizing
for (( i=1; i<=$LINES; i++ )); do echo; done; clear

#------------------------------
# Print bell character on long running commands
#------------------------------

autoload -Uz add-zsh-hook

# duration in seconds after which a bell should be sent
typeset -i LONGRUNTIME=60

# function to save time at which a command was started
save_starttime () {
  starttime=$SECONDS
}

# function to print \a if the command took longer than LONGRUNTIME
set_longrunning_alert () {
  if ((LONGRUNTIME > 0 && SECONDS - starttime >= LONGRUNTIME)); then
    print -n "\a"
  fi
}

# run save_starttime before a command is executed
add-zsh-hook preexec save_starttime

# run set_longrunning_alert after a command finishes (before the prompt)
add-zsh-hook precmd set_longrunning_alert

# opam configuration
test -r /home/akh/.opam/opam-init/init.zsh && . /home/akh/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#------------------------------
# Window title
#------------------------------

set_title_pre () {
  printf "\x1b]0;%s\x07" "$1";
}

set_title_post () {
  printf "\x1b]0;%s\x07" "$PWD"
}

add-zsh-hook preexec set_title_pre
add-zsh-hook precmd set_title_post
