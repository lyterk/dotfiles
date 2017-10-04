#!/bin/zsh

echo "############################"
echo "########### HOME ###########"
echo "############################"

dir=$HOME/dotfiles/home

for file in $dir/*; do
    base=$(basename $file)
    if [[ "$base" != "copier.sh" ]]; then
        echo .$base
        rm $HOME/.$base
        ln -s $file ~/.$base
    fi
done

# test ssh key
