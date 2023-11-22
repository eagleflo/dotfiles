autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -C "$(which aws_completer)" aws

autoload -Uz vcs_info
precmd() { vcs_info }

autoload -Uz colors && colors
export CLICOLOR=1
type vivid &> /dev/null && export LS_COLORS=$(vivid generate gruvbox-light)
type eza &> /dev/null && alias ls=eza

export ALACRITTYCFG="$HOME/.config/alacritty/alacritty.yml"

function light () {
  if [[ "$OSTYPE" == "darwin"* ]]; then
    sed -i '' -e "s#^colors: \*.*#colors: *gruvbox_light#g" ${ALACRITTYCFG:A}
  else
    sed -i -e "s#^colors: \*.*#colors: *gruvbox_light#g" ${ALACRITTYCFG:A}
  fi
}

function dark () {
  if [[ "$OSTYPE" == "darwin"* ]]; then
    sed -i '' -e "s#^colors: \*.*#colors: *gruvbox_dark#g" ${ALACRITTYCFG:A}
  else
    sed -i -e "s#^colors: \*.*#colors: *gruvbox_dark#g" ${ALACRITTYCFG:A}
  fi
}

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '!'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' formats ' (%b%u%c)'
setopt PROMPT_SUBST
PROMPT='%F{green}%~%F{reset}%F{red}${vcs_info_msg_0_}%F{reset} %# '

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

# Disable START / STOP
stty -ixon

type zoxide &> /dev/null && eval "$(zoxide init zsh)"

# Use Python 3 as Python unless it exists
! type python > /dev/null && alias python=python3.12
! type pip > /dev/null && alias pip=pip3

# nvm
# [ -d /usr/share/nvm ] && source /usr/share/nvm/init-nvm.sh
# export NVM_DIR="$HOME/.nvm"
# [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"
# [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
