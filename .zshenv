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
    ~/go/bin
    /usr/local/cuda/bin
    ~/Library/Android/sdk/tools
    ~/Library/Android/sdk/platform-tools
)

if [[ $OSTYPE == darwin* ]]; then
    path+=(
        /usr/local/texlive/2015/bin/x86_64-darwin
        "/Applications/Racket v6.6/bin"
    )

    export PGDATA=/usr/local/var/postgres
    export NODE_PATH=/usr/local/lib/node_modules
    export GOPATH=~/go
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
fi

HOMEBREW_NO_ANALYTICS=1

# OCaml & OPAM
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export DYLD_LIBRARY_PATH="/Developer/NVIDIA/CUDA-7.5/lib:/usr/local/cuda/lib:$DYLD_LIBRARY_PATH"
export CUDA_HOME="/usr/local/cuda"

# Cornerstone
export CORNERSTONE_SDK_ROOT=/opt/cornerstone-2.1.1
path+=$CORNERSTONE_SDK_ROOT/bin
