export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    ~/.node_modules/bin
    $path
    ~/.emacs.d/bin
    ~/.cargo/bin
    ~/go/bin
)

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
