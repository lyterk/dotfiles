#!/bin/zsh

echo "############################"
echo "########### WORK ###########"
echo "############################"

dir=$HOME/dotfiles/work

cd $dir

for file in $dir/*; do
    base=$(basename $file)
    if [[ "$base" != "copier.sh" ]]; then
        echo $base
        rm $HOME/.$base
        ln -s $file $HOME/.$base
    fi
done
