#!/usr/bin/env bash
set -euo pipefail

staging_dir=$HOME/.backups/
mkdir -p $staging_dir

encrypt_key=7BC4D154D6F8B0F15F5456851ABEC6FC8FFE77DC

duplicity --encrypt-key $encrypt_key \
    --exclude $HOME/.adobe \
    --exclude $HOME/.aqbanking \
    --exclude $HOME/.audacity-data \
    --exclude $HOME/.aws \
    --exclude $HOME/.backups \
    --exclude $HOME/.boot \
    --exclude $HOME/.cabal \
    --exclude $HOME/.cache \
    --exclude $HOME/.cargo \
    --exclude $HOME/.config \
    --exclude $HOME/.dbus \
    --exclude $HOME/.doom.d \
    --exclude $HOME/.elfeed \
    --exclude $HOME/.emacs.d \
    --exclude $HOME/.factorio \
    --exclude $HOME/.fltk \
    --exclude $HOME/.ghc \
    --exclude $HOME/.ghcup \
    --exclude $HOME/.gnupg \
    --exclude $HOME/.go \
    --exclude $HOME/.gphoto \
    --exclude $HOME/.hex \
    --exclude $HOME/.ipython \
    --exclude $HOME/.java \
    --exclude $HOME/.keychain \
    --exclude $HOME/.local \
    --exclude $HOME/.m2 \
    --exclude $HOME/.mail \
    --exclude $HOME/.mix \
    --exclude $HOME/.mozilla \
    --exclude $HOME/.npm \
    --exclude $HOME/.nvm \
    --exclude $HOME/.old_doom.d \
    --exclude $HOME/.old_old_doom.d \
    --exclude $HOME/.opam \
    --exclude $HOME/.password-store \
    --exclude $HOME/.pip \
    --exclude $HOME/.pki \
    --exclude $HOME/.rustup \
    --exclude $HOME/.screenlayout \
    --exclude $HOME/.slime \
    --exclude $HOME/.ssh \
    --exclude $HOME/.ssr \
    --exclude $HOME/.stack \
    --exclude $HOME/.steam \
    --exclude $HOME/.tox \
    --exclude $HOME/.urxvt \
    --exclude $HOME/.var \
    --exclude $HOME/.vcpkg \
    --exclude $HOME/.vim \
    --exclude $HOME/.vscode-oss \
    --exclude $HOME/.w3m \
    --exclude $HOME/.wine \
    --exclude $HOME/Music \
    --exclude $HOME/Videos \
    --exclude $HOME/go
    $HOME file://$staging_dir

rsync --archive --perms --compress $staging_dir backup:/backup/
