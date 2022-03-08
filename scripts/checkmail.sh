#!/usr/bin/env bash

STATE=`nmcli networking connectivity`

echo starting

if [ $STATE = 'full' ]
then
    for var in "$@"
    do
        echo "Syncing $var"
        mbsync -qq $var
    done
    mu index
    exit 0
fi
echo "No internet connection."
exit 0
