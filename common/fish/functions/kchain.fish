function kchain
    export SHELL=fish
    eval (keychain --eval --agents ssh $argv)
    export SHELL=zsh
end
