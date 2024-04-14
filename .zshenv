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
    /opt/homebrew/lib/ruby/gems/3.3.0/bin
    ~/.local/share/gem/ruby/3.3.0/bin
)

export npm_config_prefix="$HOME/.local"

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_ENV_HINTS=1
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export POWERSHELL_TELEMETRY_OPTOUT=1
