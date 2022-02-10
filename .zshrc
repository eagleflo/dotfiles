autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -C '/usr/bin/aws_completer' aws

autoload -Uz vcs_info
precmd() { vcs_info }

autoload -Uz colors && colors
export CLICOLOR=1
type vivid &> /dev/null && export LS_COLORS=$(vivid generate gruvbox-light)
type exa &> /dev/null && alias ls=exa
alias diff='diff --color'

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '!'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' (%b%u%c)'
setopt PROMPT_SUBST
PROMPT='%F{green}%~%F{reset}%F{red}${vcs_info_msg_0_}%F{reset} %# '

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000

# Disable START / STOP
stty -ixon

# nvm
[ -d /usr/share/nvm ] && source /usr/share/nvm/init-nvm.sh
