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

set RASP_GCC /usr/local/bin/aarch64-none-elf/bin
set NPM_HOME /home/kev/.npm-packages/bin

set -gx PATH $PATH $RASP_GCC $NPM_HOME
