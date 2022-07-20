if [[ `uname` == "Darwin" ]]; then
  if [ -e /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif [ -e /usr/local/bin/brew ]; then
    eval "$(/usr/local/bin/brew shellenv)"
  fi

  # Android SDK
  export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
  export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/openjdk/include"

  # Ruby
  export LDFLAGS="-L$HOMEBREW_PREFIX/opt/ruby/lib"
  CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/ruby/include"
  export PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/ruby/lib/pkgconfig"

  # Path
  path=(
    $HOMEBREW_PREFIX/opt/ruby/bin
    $path
    $HOMEBREW_PREFIX/sbin
    $HOMEBREW_PREFIX/lib/ruby/gems/3.1.0/bin
    $ANDROID_SDK_ROOT/emulator
    $ANDROID_SDK_ROOT/platform-tools
  )
fi
