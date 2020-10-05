alias sf "~/dotfiles/home/copier.py; and source ~/.config/fish/config.fish"
alias search "pacsearch"
alias pacs "sudo pacman -S"
alias pacup "sudo pacman -Syu"
alias pacr "sudo pacman -Rsn"

<<<<<<< HEAD
alias xc "xclip -selection clipboard"
=======
alias pac_no_reinstall "sudo pacman -S --needed"
>>>>>>> cbccd1b... Sudo stuff

alias i=ipython
alias python=python3
alias pipi="pip3 install --user"

alias ppush "pass git push origin master"
alias ppull "pass git pull --rebase origin master"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'

set -x BROWSER /usr/bin/firefox
set -gx EDITOR "vim"
