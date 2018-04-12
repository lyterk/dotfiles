alias spac "sudo pacman"
alias spacs "spac -S"
alias sagi "spacs"
alias spacsy "spac -Syy"
alias spacup "spac -Syu"
alias sagu "spacup"
alias spacr "spac -Rn"
alias sagr "spacr"

alias et "emacsclient -t"
alias ed "emacs --daemon &> ~/.emacs.d/logs/(date '+%m-%d-%y').log &"

alias sz "source ~/.zshrc"
alias na "ls"

set -gx BROWSER /usr/bin/firefox
set -gx EDITOR "/usr/bin/emacsclient -t"
