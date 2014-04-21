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
# DISABLE_AUTO_UPDATE="true"

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
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(osx brew git github lein node npm bower django pip ruby gem cabal)

source $ZSH/oh-my-zsh.sh

# User configuration

# Locale
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LESSCHARSET='utf-8'

# Path
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:~/.cabal/bin
if [[ $OSTYPE == darwin* ]]; then
    export PATH=$PATH:/usr/local/share/npm/bin
    export PATH=$PATH:/usr/local/opt/ruby/bin
    export PATH=$PATH:/usr/local/texlive/2013/bin/x86_64-darwin
    export PATH=$PATH:"/Applications/Racket v6.0/bin"
    export PATH=$PATH:~/go/bin
fi
export PATH=$PATH:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
export PATH=$PATH::~/src/depot_tools

if [[ $OSTYPE == linux* ]]; then
    eval `dircolors ~/.dir_colors`
fi

export CLASSPATH=.
export PYTHONSTARTUP=~/.pythonrc
export GOPATH=~/go/

if [[ $OSTYPE == darwin* ]]; then
    export PGDATA=/usr/local/var/postgres
    export NODE_PATH=/usr/local/lib/node_modules
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export SCALA_HOME="$(brew --prefix scala)"
    export GROOVY_HOME="$(brew --prefix groovy)/libexec"
    export ANDROID_HOME=/usr/local/opt/android-sdk

    # pkg-config
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/X11/lib/pkgconfig

    # Amazon EC2
    export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
    export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem | /usr/bin/head -1)"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export EC2_AMITOOL_HOME="/usr/local/Library/LinkedKegs/ec2-ami-tools/jars"
fi

# Aliases
if [[ $OSTYPE == darwin* ]]; then
    alias emacs="$(brew --prefix emacs)/Emacs.app/Contents/MacOS/Emacs -nw"
    alias vim="$(brew --prefix macvim)/MacVim.app/Contents/MacOS/vim"
fi

# Unalias gm (GraphicsMagick, not git merge)
unalias gm

# Arch
if [[ $OSTYPE == darwin* ]]; then
    export ARCHFLAGS="-arch x86_64"
fi
