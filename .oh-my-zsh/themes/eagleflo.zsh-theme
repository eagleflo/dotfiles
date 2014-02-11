PROMPT='%{$fg[green]%}%~ $(git_prompt_info)%{$reset_color%}$ '

if [[ $HOST == eagleflow ]]; then
    PROMPT='%{$fg[cyan]%}[%M]%{$reset_color%}:'$PROMPT
elif [[ $HOST == mannerheim || $HOST == paasikivi ]]; then
    PROMPT='%{$fg[magenta]%}[%M]%{$reset_color%}:'$PROMPT
fi

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}✗%{$fg[red]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[red]%})"

unset LSCOLORS
