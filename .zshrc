autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -C "$(which aws_completer)" aws

autoload -Uz vcs_info
precmd() { vcs_info }

autoload -Uz colors && colors
export CLICOLOR=1
type vivid &> /dev/null && export LS_COLORS=$(vivid generate gruvbox-dark)
type eza &> /dev/null && alias ls=eza

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '!'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' (%b%u%c)'
setopt PROMPT_SUBST
PROMPT='%F{green}%~%F{reset}%F{red}${vcs_info_msg_0_}%F{reset} %# '
if [ -n "$SSH_CLIENT" ]; then
  if [[ $(hostname) == eagleflow ]]; then
      PROMPT='%F{magenta}[%m]%F{reset} '$PROMPT
  fi
  if [[ $(hostname) == arch ]]; then
      PROMPT='%F{cyan}[%m]%F{reset} '$PROMPT
  fi
fi

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

type zoxide &> /dev/null && eval "$(zoxide init zsh)"

# Use Python 3 as Python unless it exists
! type python > /dev/null && alias python=python3.13
! type pip > /dev/null && alias pip=pip3

# fnm
type fnm &> /dev/null && eval "$(fnm env --use-on-cd)"

# task
type go-task &> /dev/null && alias task=go-task

# nvm
# [ -d /usr/share/nvm ] && source /usr/share/nvm/init-nvm.sh
# export NVM_DIR="$HOME/.nvm"
# [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"
# [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"

# Disable START / STOP
stty -ixon
