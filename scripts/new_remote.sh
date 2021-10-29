#!/usr/bin/env zsh

if [ "$EUID" -ne 0 ]
   then echo "Script must be run as root"
   exit
fi

name=$1
git init --bare /git/$name.git
chown -R git:git /git/$name.git
echo
