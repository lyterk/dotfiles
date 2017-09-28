#!/bin/zsh
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/.dotfiles/                    # dotfiles directory

cd $dir

for file in $dir/.*; do
    base=$(basename $file)
    echo $base
    rm ~/$base
    ln -s $file ~/$basename
done
