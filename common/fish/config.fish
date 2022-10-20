alias sf "python ~/dotfiles/link.py; and source ~/.config/fish/config.fish"
alias search "pacsearch"
alias pacs "sudo pacman -S"
alias pacup "sudo pacman -Syu"
alias pacr "sudo pacman -Rsn"

alias sbcl "rlwrap sbcl"
alias scu "systemctl --user"

alias xc "xclip -selection clipboard"
alias pac_no_reinstall "sudo pacman -S --needed"

alias ppush "pass git push origin mainline"
alias ppull "pass git pull --rebase origin mainline"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'
alias vim=nvim

alias remark "rsync -avzz --rsync-path=/usr/bin/rsync remarkable:/home/root/.local/share/remarkable/xochitl ~/Documents/remarkable/"

set -x BROWSER /usr/bin/firefox
set -gx EDITOR "vim"

eval keychain --agents ssh --eval $HOME/.ssh/git/id_rsa
eval keychain --agents ssh --eval $HOME/.ssh/nuc/id_rsa
source ~/.keychain/$HOSTNAME-fish

set -x RUST_SRC_PATH $HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library

ssh_agent
