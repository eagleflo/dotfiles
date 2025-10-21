export GPG_TTY=$(tty)
export CLICOLOR=1

if [[ $(uname) == "Linux" ]]; then
    export JAVA_HOME=/usr/lib/jvm/java-17-openjdk
    export ANDROID_HOME=$HOME/Android/Sdk
elif [[ $(uname) == "Darwin" ]]; then
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home
    export ANDROID_HOME=$HOME/Library/Android/sdk
fi

typeset -U path
path=(
    ~/bin
    ~/.local/bin
    /opt/homebrew/opt/python/bin
    /opt/homebrew/opt/ruby/bin
    $path
    /usr/local/sbin
    /opt/homebrew/bin
    ~/.config/emacs/bin
    ~/.cargo/bin
    ~/go/bin
    /opt/homebrew/lib/ruby/gems/3.4.0/bin
    ~/.local/share/gem/ruby/3.4.0/bin
    ~/.lmstudio/bin
    $ANDROID_HOME/emulator
    $ANDROID_HOME/platform-tools
)

export npm_config_prefix="$HOME/.local"

# export NVM_DIR="$HOME/.nvm"
# [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"
# [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] &&
#   \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_ENV_HINTS=1
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export POWERSHELL_TELEMETRY_OPTOUT=1
