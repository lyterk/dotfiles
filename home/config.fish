bind \e\b backward-kill-word

alias spac "sudo pacman"
alias spacs "spac -S"
alias sagi "spacs"
alias spacsy "spac -Syy"
alias spacup "spac -Syu"
alias sagu "spacup"
alias spacr "spac -Rn"
alias sagr "spacr"

alias xcop "xclip -selection clipboard"
alias xpa "xclip -selection clipboard -o"

alias et "emacsclient -t"
alias ed "emacs --daemon > ~/.emacs.d/logs/(date '+%m-%d-%y').log &"

alias sz "source ~/.zshrc"
alias na "ls"

set -gx BROWSER /usr/bin/firefox
set -gx EDITOR "/usr/bin/emacsclient -t"
set -gx GOPATH $HOME/.local/go

set RASP_GCC /usr/local/bin/aarch64-none-elf/bin
set NPM_HOME /home/kev/.npm-packages/bin
set CARGO $HOME/.cargo/bin
set BASE_PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/lib/jvm/default/bin /usr/bin/site_perl /usr/bin/vendor_perl /usr/bin/core_perl

set -gx PATH $BASE_PATH $RASP_GCC $NPM_HOME $CARGO $GOPATH/bin
