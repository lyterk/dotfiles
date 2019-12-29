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
alias ed "emacs --daemon > $HOME/.emacs.d/logs/(date '+%m-%d-%y').log &"

alias sz "source $HOME/.zshrc"
alias na "ls"

alias ppush "pass git push origin master"
alias ppull "pass git pull origin master"

alias vre "sudo systemctl restart openvpn-client@US_Seattle.service"
alias vsto "sudo systemctl stop openvpn-client@US_Seattle.service"
alias vsta "sudo systemctl start openvpn-client@US_Seattle.service"

set -x GTK_IM_MODULE "fcitx"
set -x QT_IM_MODULE "fcitx"
set -x XMODIFIERS "@im=fcitx"
set -gx GOPATH $HOME/code/golang
set -gx DART_PATH $HOME/.pub-cache/bin
# set -gx CHICKEN_REPOSITORY_PATH $HOME/.local/share/chicken-eggs
set -gx CHICKEN_REPOSITORY $HOME/.local/share/chicken-eggs
set -gx BOOT_CLOJURE_VERSION 1.10.1

set -gx BROWSER /usr/bin/firefox
set -gx EDITOR "/usr/bin/emacsclient -t"
set -gx GOBIN $GOPATH/bin
set -gx LOCALBIN $HOME/.local/bin
set -gx SNAP_BIN /snap/bin

set FLUTTER_BIN $HOME/.local/share/flutter/bin
# set RASP_GCC /usr/local/bin/aarch64-none-elf/bin

set -x XDG_DATA_DIRS /usr/share:/usr/share:/usr/local/share

set NPM_HOME $HOME/.npm-packages/bin
set CARGO $HOME/.cargo/bin
set NPM $HOME/.local/share/npm-packages
set MINICONDA $HOME/.miniconda/bin
set BASE_PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/lib/jvm/default/bin /usr/bin/site_perl /usr/bin/vendor_perl /usr/bin/core_perl /usr/games


set -gx PATH $MINICONDA $BASE_PATH $EMACS_PATH $RASP_GCC $NPM_HOME $CARGO $GOPATH/bin $LOCALBIN $SNAP_BIN $FLUTTER_BIN $NPM/bin
set -gx MANPATH $NPM/share/man
set -gx NODE_PATH $NPM/lib/node_modules $NODE_PATH

function fish_title
    true
end

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end
