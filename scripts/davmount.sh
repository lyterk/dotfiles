#! /usr/bin/env bash

if [ -z "$1" -o -z "$2" -o -z "$SUDO_UID" -o -z "$SUDO_USER" ]; then
    echo 'Usage: sudo' $(basename $0) '<url> <mountpoint>'
    echo '       sudo' $(basename $0) '-u <mountpoint>'
    exit 1
fi

set -eu

GPG_OPTS="-q --no-verbose --no-tty --no-permission-warning"

URL=$1
DIR=$(readlink -f "$2")

HOST=$(
    echo $URL |
    sed -n 's#^http[s]\?://\([a-z0-9._-]\+\)/.*#\1#ip' |
    tr [:upper:] [:lower:]
)
SECRETS_FILE=$HOME/.passwords/dav2fs.txt.gpg

USER_HOME=$(getent passwd $SUDO_USER | cut -d: -f6)
GPG_HOME="$USER_HOME/.gnupg"
DAVMOUNT_HOME="$USER_HOME/.passwords"

if [ -z "$USER_HOME" ]; then
    echo No home directory set for $SUDO_USER
    exit 1
fi

if [[ $URL = -u ]]; then
    umount "$DIR"
elif [[ ! -f "$SECRETS_FILE" ]]; then
    echo "Couldn't find secrets file in $SECRETS_FILE"
    exit 1
else
    SECRETS="$DAVMOUNT_HOME/$$.secrets"
    trap "rm -f \"$SECRETS\"" EXIT
    mkfifo -m 600 "$SECRETS"

    (
        echo -n "\"$URL\" "
        gpg --homedir "$GPG_HOME" $GPG_OPTS -d "$SECRETS_FILE"
    ) > "$SECRETS" &

    mount.davfs -o "uid=$SUDO_UID",conf=<(
        if [ -f "$USER_HOME/.davfs2/davfs2.conf" ]; then
            cat "$USER_HOME/.davfs2/davfs2.conf"
        fi
        echo secrets $SECRETS
    ) "$URL" "$DIR"
fi
