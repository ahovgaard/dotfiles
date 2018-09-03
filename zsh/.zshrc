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
