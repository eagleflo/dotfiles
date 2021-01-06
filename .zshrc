# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="eagleflo"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(osx git github lein node npm)

source $ZSH/oh-my-zsh.sh

# User configuration

# Locale
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

export PYTHONSTARTUP=~/.pythonrc

export GPG_TTY=$(tty)

# Unalias gm (GraphicsMagick, not git merge)
unalias gm

# Disable START / STOP
stty -ixon

typeset -U path
path=(
    ~/bin
    ~/.local/bin
    ~/.cabal/bin
    ~/.cargo/bin
    ~/.npm/bin
    ~/.node_modules/bin
    ~/.fnm
    ~/.dotnet/tools
    ~/.emacs.d/bin
    ~/.gem/ruby/2.7.0/bin
    /usr/local/opt/ruby/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /usr/sbin
    /bin
    /sbin
    $path
)

if [[ $OSTYPE == darwin* ]]; then
    path+=(
        /usr/local/lib/ruby/gems/2.7.0/bin
        /usr/local/texlive/2017/bin/x86_64-darwin
        ~/Library/Android/sdk/tools
        ~/Library/Android/sdk/platform-tools
        ~/.fastlane/bin
        "/Applications/Racket v7.6/bin"
    )

    export PGDATA=/usr/local/var/postgres
    export NODE_PATH=/usr/local/lib/node_modules
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export SCALA_HOME="$(brew --prefix scala)"
    export ANDROID_HOME=~/Library/Android/sdk
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
    export BOOST_ROOT=$(brew --prefix boost)

    # Amazon EC2
    export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
    export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem | /usr/bin/head -1)"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export EC2_AMITOOL_HOME="/usr/local/Library/LinkedKegs/ec2-ami-tools/jars"

    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

    HOMEBREW_NO_ANALYTICS=1
fi

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval "$(fnm env)"

DOTNET_CLI_TELEMETRY_OPTOUT=1
POWERSHELL_TELEMETRY_OPTOUT=1
