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

if [[ `uname` == "Darwin" ]]; then
  # Android SDK
  export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
  export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
  export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
  export CPPFLAGS="-I/usr/local/opt/openjdk/include"

  # Ruby
  export LDFLAGS="-L/usr/local/opt/ruby/lib"
  CPPFLAGS+=" -I/usr/local/opt/ruby/include"
  export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"
fi


export npm_config_prefix="$HOME/.local"

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
