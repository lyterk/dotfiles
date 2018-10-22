alias sag "sudo apt"
alias sagi "sag install"
alias sagu "sag update"
alias saga "sag autoremove"
alias sagr "sag remove"
alias acs "apt-cache search"
alias acm "apt-cache madison"
alias svba "source venv/bin/activate"
alias sf "~/dotfiles/work/copier.py; and source ~/.config/fish/config.fish"

alias bb "brazil-build"
alias bre "brazil-runtime-exec"
alias bte "brazil-test-exec"
alias brep "bre python bin/manage.py"
alias bwsm "brazil ws --sync --md"

alias python=python3

alias la 'ls -lAFh'   #long list,show almost all,show type,human readable
alias lr 'ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lt 'ls -ltFh'   #long list,sorted by date,show type,human readable

alias mon 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U fba_analysis'
alias monr 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U root'
alias gol 'psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells 'psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias ags 'psql -h agsanalytics01.cuv0wvsrttvh.us-east-1.redshift.amazonaws.com -p 8192 -d agsanalytics -U ags_root'
alias wb "~/.local/share/sql_workbench/sqlworkbench.sh &"
alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"

alias ppush "pass git push origin master"
alias ppull "pass git pull origin master"

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

set -gx AIRFLOW_HOME /Users/kllyter/airflow

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"
set -x GOHOME /Users/kllyter/go
set -x GOBIN $GOHOME/bin
set -x NVM_DIR /Users/kllyter/.nvm

# Java home is gonna have to be a little weird to keep workbench working
# Using jenv to set this now. Sigh.
# set -x JAVA_HOME /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/
set -x JAVA_HOME (/usr/libexec/java_home -v 1.8.0)
set -x LOCAL_BIN $HOME/.local/bin
set -x CONDA_BIN $HOME/.miniconda/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x SDETOOLS /apollo/env/SDETools/bin

set -x NODE_PATH /usr/local/opt/node@8/bin

set -x RUST_BIN $HOME/.cargo/bin
set -x BASE_PATH /usr/local/bin /usr/bin /bin /usr/sbin /sbin /usr/local/MacGPG2/bin

set -gx PATH $CONDA_BIN $BASE_PATH $TOOLBOX $JDK_BIN $LOCAL_BIN $RUST_SRC_PATH $RUST_BIN $SDETOOLS $OCTANE $NODE_PATH $GOBIN

set -gx SHELL /usr/local/bin/fish
set -gx EDITOR vim

function sync_dots
    rsync -a --cvs-exclude ~/dotfiles/ desk:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ d:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ e:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ f:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ hcary:~/dotfiles
end
