#!/bin/zsh

dir=~/dotfiles/                    # dotfiles directory
echo "############################"
echo "########### HOME ###########"
echo "############################"

dir=~/.dotfiles/                    # dotfiles directory

cd $dir

for file in $dir/.*; do
    base=$(basename $file)
    if [[ "$base" != ".git" ]]; then
           echo $base
           rm ~/$base
           ln -s $file ~/$basename
    fi
done

# This will be different in the branch
