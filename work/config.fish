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

alias i=ipython
alias python=python3
alias pipi="pip3 install --user"

alias mon 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U fba_analysis'
alias monr 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U root'
alias gol 'psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells 'psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias ags 'psql -h agsanalytics01.cuv0wvsrttvh.us-east-1.redshift.amazonaws.com -p 8192 -d agsanalytics -U ags_root'
alias wb "~/.local/share/sql_workbench/sqlworkbench.sh &"
alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"
alias d ddgr
alias g googler
alias python python3

alias pahvo 'psql -h pahvo.cakudculty6n.us-east-1.rds.amazonaws.com -p 5432 -d pahvo -U airflow_test_user'

alias ppush "pass git push origin master"
alias ppull "pass git pull origin master"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'

set -gx AIRFLOW_HOME $HOME/airflow

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"
set -x GOPATH $HOME/code/go
set -x GOBIN $GOPATH/bin

# set -x NODE_PATH $HOME/.nvm/versions/node/v8.12.0/bin

# set -x JAVA_HOME (/usr/libexec/java_home -v 1.8.0)
set -x LOCAL_BIN $HOME/.local/bin
# set -x CONDA_BIN $HOME/.miniconda/bin
set -x TOOLBOX $HOME/.toolbox/bin
# set -x SDETOOLS /apollo/env/SDETools/bin

set -x GO_HOME /usr/lib/go-1.11/
set -x EMACS_PATH $HOME/.emacs.d/bin
set -x RUST_BIN $HOME/.cargo/bin
set -x BASE_PATH /usr/local/bin /usr/bin /bin /usr/sbin /sbin

set -gx PATH $BASE_PATH $LOCAL_BIN $RUST_BIN $EMACS_PATH $TOOLBOX $GO_HOME/bin $GOBIN

set -gx SHELL /usr/bin/fish
set -gx EDITOR "emacsclient -t"

function sync_dots
    rsync -a --cvs-exclude ~/dotfiles/ desk:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ d:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ e:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ f:~/dotfiles
end

alias rsync_intern='rsync --progress -a --exclude .git --exclude __pycache__ ~/internal-api/src/FBAAirflowInternalServices/ desk:~/internal_api/src/FBAAirflowInternalServices/'
# fswatch -o /Users/kllyter/internal_api/src/FBAAirflowInternalServices | while read f; rsync_intern; end
