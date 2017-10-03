#!/bin/zsh

echo "############################"
echo "########### HOME ###########"
echo "############################"

dir=~/.dotfiles/                    # dotfiles directory

cd $dir

for file in $dir/.*; do
    base=$(basename $file)
    echo $base
    rm ~/$base
    ln -s $file ~/$basename
done
