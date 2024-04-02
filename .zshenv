export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    ~/bin
    ~/.local/bin
    $path
    /usr/local/sbin
    ~/.emacs.d/bin
    ~/.cargo/bin
    ~/go/bin
    ~/.local/share/gem/ruby/3.0.0/bin
    ~/.mozbuild/git-cinnabar
)

export npm_config_prefix="$HOME/.local"

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
