export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    ~/bin
    ~/.node_modules/bin
    $path
    ~/.emacs.d/bin
    ~/.cargo/bin
    ~/go/bin
    ~/.mozbuild/git-cinnabar
)

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
