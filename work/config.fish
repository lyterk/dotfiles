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

alias ls 'exa'
alias la 'exa -laFh'   #long list,show almost all,show type,human readable
alias lr 'exa -tRFh'   #sorted by date,recursive,show type,human readable
alias lt 'exa -ltFh'   #long list,sorted by date,show type,human readable

alias mon 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U fba_analysis'
alias monr 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U root'
alias gol 'psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells 'psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias ags 'psql -h agsanalytics01.cuv0wvsrttvh.us-east-1.redshift.amazonaws.com -p 8192 -d agsanalytics -U ags_root'
alias wb "~/.local/share/sql_workbench/sqlworkbench.sh &"
alias emacsclient "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"
alias d ddgr
alias g googler

alias pahvo 'psql -h pahvo.cakudculty6n.us-east-1.rds.amazonaws.com -p 5432 -d pahvo -U airflow_test_user'

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
set -x NODE_PATH $HOME/.nvm/versions/node/v8.12.0/bin

# Java home is gonna have to be a little weird to keep workbench working
# Using jenv to set this now. Sigh.
# set -x JAVA_HOME /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/
set -x EMACS_HOME /Applications/Emacs.app/Contents/MacOS/bin
set -x JAVA_HOME (/usr/libexec/java_home -v 1.8.0)
set -x LOCAL_BIN $HOME/.local/bin
set -x CONDA_BIN $HOME/.miniconda/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x SDETOOLS /apollo/env/SDETools/bin

set -x RUST_BIN $HOME/.cargo/bin
set -x BASE_PATH /usr/local/bin /usr/bin /bin /usr/sbin /sbin /usr/local/MacGPG2/bin

set -gx PATH $EMACS_HOME $CONDA_BIN $BASE_PATH $TOOLBOX $JDK_BIN $LOCAL_BIN $RUST_SRC_PATH $RUST_BIN $SDETOOLS $OCTANE $NODE_PATH $GOBIN

set -gx SHELL /usr/local/bin/fish
set -gx EDITOR vim

function sync_dots
    rsync -a --cvs-exclude ~/dotfiles/ desk:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ d:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ e:~/dotfiles
    rsync -a --cvs-exclude ~/dotfiles/ f:~/dotfiles
end

function dbash
    sudo docker exec -it (echo (sudo docker ps | grep $argv | cut -d " " -f1)) bash
end

alias rsync_intern='rsync --progress -a --exclude .git --exclude __pycache__ ~/internal-api/src/FBAAirflowInternalServices/ desk:~/internal_api/src/FBAAirflowInternalServices/'
# fswatch -o /Users/kllyter/internal_api/src/FBAAirflowInternalServices | while read f; rsync_intern; end


# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.fish
