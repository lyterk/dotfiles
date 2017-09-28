source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
antigen bundle command-not-found

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme gianu

# PATH=$(getconf PATH)

alias spac="sudo pacman"
alias spacs="spac -S"
alias sagi="spacs"
alias spacsy="spac -Syy"
alias spacup="spac -Syu"
alias sagu="spacup"
alias spacr="spac -Rn"
alias sagr="spacr"

alias sz="source ~/.zshrc"
alias na="ls"

BROWSER=/usr/bin/firefox
EDITOR="/usr/bin/emacsclient -t"

source ~/.zshenv
antigen apply

