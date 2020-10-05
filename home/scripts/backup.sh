#!/usr/bin/env bash
set -euo pipefail

staging_dir=$HOME/.backup-staging
mkdir -p $staging_dir

encrypt_key=7BC4D154D6F8B0F15F5456851ABEC6FC8FFE77DC
sign_key=CD3397AFBB9848841EB8A0574FAC48A86C1B43BB
export SIGN_PASSPHRASE="netflix_another_life"

duplicity --encrypt-key $encrypt_key \
    --exclude $HOME/.cache/ \
    --exclude $HOME/.gimp-2.8/ \
    --exclude $HOME/.gnupg/ \
    --exclude $HOME/.go/ \
    --exclude $HOME/.mozilla/ \
    --exclude $HOME/.password-store/ \
    --exclude $HOME/.pki/ \
    --exclude $HOME/.ssh/ \
    --exclude $HOME/.urxvt/ \
    --exclude $HOME/.var/ \
    --exclude $HOME/.vim/ \
    --exclude $staging_dir \
    --exclude $HOME/Videos/ \
    $HOME file://$staging_dir

rsync -avpz $staging_dir nuc:/mnt/toshiba/backups-asus/
