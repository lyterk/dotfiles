#!/bin/zsh

echo "############################"
echo "########### HOME ###########"
echo "############################"

$dir=~/dotfiles

for file in $dir/.*; do
    base=$(basename $file)
    if [[ "$base" != ".git" ]]; then
           echo $base
           rm ~/$base
           ln -s $file ~/$basename
    fi
done

# This was the thing on the master branch

# This was the thing on the master branch too

# Third thing on the master branch
