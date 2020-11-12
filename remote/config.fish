alias sag "sudo yum"
alias sagi "sag install"
alias sagu "sag update"
alias saga "sag autoremove"
alias sagr "sag remove"
alias svba "source venv/bin/activate"
alias sf "~/dotfiles/work/copier.py; and source ~/.config/fish/config.fish"

alias bb "brazil-build"
alias bbr "bb release"
alias bre "brazil-runtime-exec"
alias bte "brazil-test-exec"
alias brep "bre python bin/manage.py"
alias bwsm "brazil ws --sync --md"

alias i ipython
alias python python3
alias pipi "pip3 install --user"

alias mon 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U fba_analysis'
alias monr 'psql -h fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d monster -U root'
alias gol 'psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells 'psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias ags 'psql -h agsanalytics01.cuv0wvsrttvh.us-east-1.redshift.amazonaws.com -p 8192 -d agsanalytics -U ags_root'
alias fare 'psql -h fare-bi-rs1.cejlc1wjv9tl.us-east-1.redshift.amazonaws.com -p 8192 -d farebirs1 -U dw_virtdashboard_admin'
alias wb "~/.local/share/sql_workbench/sqlworkbench.sh &"
alias edae "emacs --daemon > ~/.emacs.d/log/'(date '+%m-%d-%y')'.log & "
alias et "emacsclient -t"
alias vipy "$VIRTUAL_ENV/bin/ipython"
alias d ddgr
alias g googler
alias python python3
alias pip $HOME/.pyenv/shims/pip3
alias csi "rlwrap csi"

alias bnu "brazil ws create --name"
alias bp "brazil ws use -p"
alias bv "brazil ws use -vs"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'

# Fix mwinit issue
alias mwinit "env LD_LIBRARY_PATH= mwinit -o"

set -gx AIRFLOW_HOME $HOME/airflow

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"

set -x RUST_SRC_PATH $HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src


set -x ENV_IMPROVEMENT /apollo/env/envImprovement/bin
set -x LOCALBIN $HOME/.local/bin
set -x CARGOPATH $HOME/.cargo/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x PYENV $HOME/.pyenv/bin
set -x LINUXBREW /home/linuxbrew/.linuxbrew/bin
set -x GOPATH $HOME/go
set -x SNAPPATH /snap/bin
set -x G2S2 /apollo/env/G2S2CommandTools/bin

set -gx EDITOR vim

set -gx BASE_PATH /usr/NX/bin /usr/local/bin /bin /usr/bin /home/kllyter/bin /usr/local/sbin /usr/sbin /sbin
set -gx PATH $BASE_PATH $ENV_IMPROVEMENT $GOBIN $LOCALBIN $CARGOPATH $TOOLBOX $G2S2

function smart_zgrep
    find (pwd) -type f -exec echo {} \; -exec zgrep $1 {} \; > /tmp/find_results.txt
end
