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
alias bbr "bb release"
alias bre "brazil-runtime-exec"
alias bte "brazil-test-exec"
alias brep "bre python bin/manage.py"
alias bwsm "brazil ws --sync --md"

alias i=ipython
alias python=python3
alias pipi="pip3 install --user"

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

alias ssam "sudo /home/linuxbrew/.linuxbrew/bin/sam"

alias ppush "pass git push origin mainline"
alias ppull "pass git pull --rebase origin mainline"

set -x SOUND_FOLDER "$HOME/Music"
alias pat 'play $SOUND_FOLDER/right_answer.mp3 2> /dev/null'
alias poke 'play $SOUND_FOLDER/wrong_answer.mp3 2> /dev/null'

set -gx AIRFLOW_HOME $HOME/airflow

set -x BROWSER /usr/bin/firefox
set -x EDITOR "/usr/bin/emacsclient -t"

set -x RUST_SRC_PATH $HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

set -x LOCALBIN $HOME/.local/bin
set -x CARGOPATH $HOME/.cargo/bin
set -x TOOLBOX $HOME/.toolbox/bin
set -x PYENV $HOME/.pyenv/bin
set -x LINUXBREW /home/linuxbrew/.linuxbrew/bin
set -x GOPATH $HOME/go

set -gx SHELL /usr/bin/fish
set -gx EDITOR "emacsclient -t"

set -gx PATH $PATH $LOCALBIN $CARGOPATH $TOOLBOX $PYENV $LINUXBREW

status --is-interactive; and source (pyenv init -|psub)

function samall
    ssam build --use-container --build-dir .sudo-aws-sam/
    ssam package --s3-bucket sam-dev-kllyter --template-file .sudo-aws-sam/template.yaml --output-template packaged.yaml
    ssam deploy --template-file packaged.yaml --region us-west-2 --capabilities CAPABILITY_IAM --stack-name $argv
end

function amall
    sam build --use-container --build-dir .aws-sam/
    sam package --s3-bucket sam-dev-kllyter --template-file .aws-sam/template.yaml --output-template packaged.yaml
    sam deploy --template-file packaged.yaml --region us-west-2 --capabilities CAPABILITY_IAM --stack-name $argv
end



function sync_dots
    rsync -a --cvs-exclude ~/dotfiles/ desk:~/dotfiles &
end

set -x sdet /workspace/sde_integ/src/SDEIntegrationTests
set -x sde /workspace/sde/src/SellerDiscountsEvaluatorService
set -x sdm /workspace/sde/src/SellerDiscountsManagerService
set -x rudra /workspace/rudra/src/Rudra
set -x ocean /workspace/ocean/src/Ocean
set -x odyssey /workspace/odyssey/src/TheOdyssey
set -x rbs /workspace/rbs/src/RecurringBillingService
