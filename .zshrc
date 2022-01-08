autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -C '/usr/bin/aws_completer' aws

autoload -Uz vcs_info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats ' (%b)'
setopt PROMPT_SUBST
PROMPT='%~${vcs_info_msg_0_} %# '
