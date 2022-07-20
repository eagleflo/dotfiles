export GPG_TTY=$(tty)
export CLICOLOR=1

typeset -U path
path=(
    ~/bin
    ~/.local/bin
    /usr/local/opt/ruby/bin
    $path
    ~/.emacs.d/bin
    ~/.cargo/bin
    ~/go/bin
    ~/.mozbuild/git-cinnabar
    /usr/local/sbin
    /usr/local/lib/ruby/gems/3.1.0/bin
)


# Ruby
export LDFLAGS="-L/usr/local/opt/ruby/lib"
CPPFLAGS+=" -I/usr/local/opt/ruby/include"
export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"

export npm_config_prefix="$HOME/.local"

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
