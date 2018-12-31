path=(
    ~/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /usr/sbin
    /bin
    ~/.cabal/bin
    ~/.npm/bin
    ~/.cargo/bin
)

if [[ $OSTYPE == darwin* ]]; then
    path+=(
        ~/.fastlane/bin
        /usr/local/cuda/bin
        ~/Library/Android/sdk/tools
        ~/Library/Android/sdk/platform-tools
        /usr/local/texlive/2015/bin/x86_64-darwin
        "/Applications/Racket v6.12/bin"
        /Library/Frameworks/Mono.framework/Versions/Current/bin
    )

    export PGDATA=/usr/local/var/postgres
    export NODE_PATH=/usr/local/lib/node_modules
    export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"
    export SCALA_HOME="$(brew --prefix scala)"
    export ANDROID_HOME=~/Library/Android/sdk
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
    export BOOST_ROOT=$(brew --prefix boost)

    # Amazon EC2
    export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
    export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem | /usr/bin/head -1)"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export EC2_AMITOOL_HOME="/usr/local/Library/LinkedKegs/ec2-ami-tools/jars"

    HOMEBREW_NO_ANALYTICS=1
elif [[ $OSTYPE == linux* ]]; then
    path+=(
        ~/Android/Sdk/tools
        ~/Android/Sdk/platform-tools
    )
    export ANDROID_HOME=~/Android/Sdk
fi

export GPG_TTY=$(tty)

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
