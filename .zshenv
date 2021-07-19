source "$HOME/.cargo/env"
export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    $path
    ~/.emacs.d/bin
)

autoload -Uz vcs_info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats ' (%b)'
setopt PROMPT_SUBST
PROMPT='%1~${vcs_info_msg_0_} %# '

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
