PATH=$(getconf PATH)

BROWSER=/usr/bin/firefox
EDITOR="/usr/bin/emacsclient -t"

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/jre
export LOCAL_BIN=$HOME/.local/bin
export CONDA_BIN=$HOME/.anaconda3/bin

export RUST_SRC_PATH=$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
export RUST_BIN=$HOME/.cargo/bin
export PATH=$CONDA_BIN:$PATH:$JAVA_HOME:$LOCAL_BIN:$RUST_SRC_PATH:$RUST_BIN
