path=(
    ~/bin
    /usr/local/bin
    /usr/local/sbin
    /usr/bin
    /usr/sbin
    ~/.cabal/bin
    ~/.npm/bin
    /usr/local/texlive/2015/bin/x86_64-darwin
    /usr/local/cuda/bin
    "/Applications/Racket v6.2.1/bin"
)

if [[ $OSTYPE == darwin* ]]; then
    export PGDATA=/usr/local/var/postgres
    export NODE_PATH=/usr/local/lib/node_modules
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export SCALA_HOME="$(brew --prefix scala)"
    export ANDROID_HOME=/usr/local/opt/android-sdk
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
    export BOOST_ROOT=$(brew --prefix boost)

    # Amazon EC2
    export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
    export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem | /usr/bin/head -1)"
    export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
    export EC2_AMITOOL_HOME="/usr/local/Library/LinkedKegs/ec2-ami-tools/jars"
fi

# OCaml & OPAM
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# Cornerstone
export CORNERSTONE_SDK_ROOT=/opt/cornerstone-2.1.0
path+=$CORNERSTONE_SDK_ROOT/bin
