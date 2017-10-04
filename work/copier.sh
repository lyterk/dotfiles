#!/bin/zsh
echo "############################"
echo "########### WORK ###########"
echo "############################"

########## Variables

dir=~/.dotfiles/work/                    # dotfiles directory

cd $dir

for file in $dir/.*; do
    base=$(basename $file)
    echo $base
    rm ~/$base
    ln -s $file ~/$base
done
