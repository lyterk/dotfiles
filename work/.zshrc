# Set up the prompt

source $HOME/.local/bin/antigen.zsh
Making 
# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pip
antigen bundle command-not-found
antigen bundle zsh-users/zsh-completions

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme gianu

# Tell Antigen that you're done.
antigen apply


setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system

alias sag="sudo apt"
alias sagi="sag install"
alias sagu="sag update"
alias saga="sag autoremove"
alias sagr="sag remove"
alias acs="apt-cache search"
alias acm="apt-cache madison"
alias svba="source venv/bin/activate"
alias sz="source ~/.zshrc"

alias la='ls -lAFh'   #long list,show almost all,show type,human readable
alias lr='ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lt='ls -ltFh'   #long list,sorted by date,show type,human readable
alias svm="vboxmanage startvm 'Amazon Linux 64-bit' --type headless"
alias mon='psql -h fba-analysis-monster.db.amazon.com -p 8192 -d monster -U fba_analysis'
alias monroot='psql -h fba-analysis-monster.db.amazon.com -p 8192 -d monster -U root'
alias gol='psql -h fba-analysis-goldman.db.amazon.com -p 8192 -d goldman -U root'
alias wells='psql -h fba-wells.c5g1ihldokmx.us-east-1.redshift.amazonaws.com -p 8192 -d wells -U root'
alias wb="~/.local/share/sql_workbench/sqlworkbench.sh &"
alias vboxnewip="VBoxManage guestproperty get b7e0428d-6059-4431-b3e4-f7cba0b0ddbe /VirtualBox/GuestInfo/Net/0/V4/IP"
alias desk="$(vboxnewip | cut -c7-)"
alias tunnel="ssh -L 3306:fbafinance-metrics.cdekw264nrry.us-west-2.rds.amazonaws.com"
alias finance="mysql -h fbafinance-metrics.cdekw264nrry.us-west-2.rds.amazonaws.com ssof -u kllyter -pbetterbetterbe -P 3306"

SOUND_FOLDER="$HOME/Music"
alias pat='(play $HOME/Music/right_answer.mp3 2> /dev/null &)'
alias poke='(play $HOME/Music/wrong_answer.mp3 2> /dev/null &)'

sme () {
    ssh kllyter@$1;
}

psql_alias () {
    eval 'psql -h $1 -p 8192 --dbname $2 --username $3';
}

monster () {
    eval 'psql_alias fba-analysis.c5g1ihldokmx.us-east-1.redshift.amazonaws.com monster $1 -f inboundops.psql'
}

goldman () {
    eval 'psql_alias fba-goldman.c5g1ihldokmx.us-east-1.redshift.amazonaws.com goldman root'
}


export NVM_DIR="/home/local/ANT/kllyter/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# eval "$(pyenv init -)"
# pyenv virtualenvwrapper

REMOTE_DOCUMENTS=/home/kllyter/Documents

encrypt_journal () {
    CURR=$(pwd)
    cd ~/Documents
    FILE_NAME=personal_$(date +"%Y-%m-%d").tar.gz
    tar -czf $FILE_NAME personal;
    rm -rf personal;
    gpg -e -r kllyter $FILE_NAME;
    rm $FILE_NAME;
    cp $FILE_NAME.gpg personal_backups
    cd $CURR
}

decrypt_journal () {
    CURR=$(pwd)
    cd ~/Documents
    DECRYPTEE=$(ls -t personal_20*) | head -1
    gpg -d -o personal.tar.gz $DECRYPTEE
    rm $DECRYPTEE;
    tar -xvf personal.tar.gz personal;
    rm -rf personal.tar.gz;
    cd $CURR
}
