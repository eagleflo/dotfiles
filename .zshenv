export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    ~/bin
    ~/.local/bin
    $path
    /usr/local/sbin
    /opt/homebrew/bin
    ~/.config/emacs/bin
    ~/.cargo/bin
    ~/go/bin
    ~/.local/share/gem/ruby/3.0.0/bin
    ~/.mozbuild/git-cinnabar
)

export npm_config_prefix="$HOME/.local"

HOMEBREW_NO_ANALYTICS=1
HOMEBREW_NO_ENV_HINTS=1
DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
