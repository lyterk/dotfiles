alias i ipython
alias python python3
alias pipi "pip3 install --user"

alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"
alias d ddgr
alias g googler
alias python python3
alias pip $HOME/.pyenv/shims/pip3

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'

# Fix mwinit issue
alias mwinit "env LD_LIBRARY_PATH= mwinit -o"

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"

set -x RUST_SRC_PATH $HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src


set -x LOCALBIN $HOME/.local/bin
set -x CARGOPATH $HOME/.cargo/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x PYENV $HOME/.pyenv/bin
set -x GOPATH $HOME/go
set -x SNAPPATH /snap/bin

set -gx EDITOR vim

set -gx BASE_PATH /usr/NX/bin /usr/local/bin /bin /usr/bin /home/kllyter/bin /usr/local/sbin /usr/sbin /sbin
set -gx PATH $BASE_PATH $GOBIN $LOCALBIN $CARGOPATH
