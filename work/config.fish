alias sag "sudo apt"
alias sagi "sag install"
alias sagu "sag update"
alias saga "sag autoremove"
alias sagr "sag remove"
alias acs "apt-cache search"
alias acm "apt-cache madison"
alias svba "source venv/bin/activate"
alias sz "source ~/dotfiles/work/copier.sh; and source ~/.zshrc"

alias bb "brazil-build"
alias bre "brazil-runtime-exec"
alias brep "bre python bin/manage.py"
alias bwsm "brazil ws --sync --md"

alias python=python3

alias la 'ls -lAFh'   #long list,show almost all,show type,human readable
alias lr 'ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lt 'ls -ltFh'   #long list,sorted by date,show type,human readable

alias svm "vboxmanage startvm 'Amazon Linux 64-bit' --type headless"
alias mon 'psql -h fba-analysis-monster.db.amazon.com -p 8192 -d monster -U fba_analysis'
alias monroot 'psql -h fba-analysis-monster.db.amazon.com -p 8192 -d monster -U root'
alias gol 'psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells 'psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias wb "~/.local/share/sql_workbench/sqlworkbench.sh &"
alias vboxnewip "VBoxManage guestproperty get 91e28c12-a3da-41c1-96f9-a68ba79579b3 /VirtualBox/GuestInfo/Net/0/V4/IP"
alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $HOME/Music/right_answer.mp3 2> /dev/null'
alias poke 'play $HOME/Music/wrong_answer.mp3 2> /dev/null'

# Test if fisher is installed, if not install
if not type -q fisher
    curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
else
    # fisher pipenv
    # fisher pisces
end

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"

set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk/jre
set -x LOCAL_BIN $HOME/.local/bin
set -x CONDA_BIN $HOME/.anaconda3/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x SDETOOLS /apollo/env/SDETools/bin
set -x OCTANE /apollo/env/OctaneBrazilTools/bin

set -x RUST_SRC_PATH $HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
set -x RUST_BIN $HOME/.cargo/bin
set -x BASE_PATH /usr/local/bin /usr/bin /bin /usr/sbin /sbin /usr/local/MacGPG2/bin

set -gx PATH $BASE_PATH $TOOLBOX $JAVA_HOME $LOCAL_BIN $RUST_SRC_PATH $RUST_BIN $CONDA_BIN $SDETOOLS $OCTANE
