bind \e\b backward-kill-word

alias pp "pass git push origin master"
alias pl "pass git pull origin master"

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
alias ls exa
alias na "ls"

alias ppush "pass git push origin master"
alias ppull "pass git pull origin master"

alias vre "sudo systemctl restart openvpn-client@client.service"
alias vsto "sudo systemctl stop openvpn-client@client.service"
alias vsta "sudo systemctl start openvpn-client@client.service"

set -x GTK_IM_MODULE "fcitx"
set -x QT_IM_MODULE "fcitx"
set -x XMODIFIERS "@im=fcitx"
set -gx GOPATH ~/code/golang
set -gx DOOMPATH ~/.emacs.d/bin

set -gx BROWSER /usr/bin/firefox
set -gx EDITOR "/usr/bin/emacsclient -t"
set -gx GOBIN $GOPATH/bin
set -gx LOCALBIN $HOME/.local/bin

set EMACS_PATH /home/kev/.emacs.d/bin
set RASP_GCC /usr/local/bin/aarch64-none-elf/bin
set NPM_HOME /home/kev/.npm-packages/bin
set CARGO $HOME/.cargo/bin
set BASE_PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/lib/jvm/default/bin /usr/bin/site_perl /usr/bin/vendor_perl /usr/bin/core_perl

set -gx PATH $BASE_PATH $EMACS_PATH $RASP_GCC $NPM_HOME $CARGO $GOPATH/bin $LOCALBIN $GOBIN $DOOMPATH
