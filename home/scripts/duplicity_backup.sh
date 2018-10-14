#!/bin/bash

export GPG_KEY=84302C86569002E023AF4B83DF0567A05B291E1C
export GOOGLE_DRIVE_SETTINGS=~/.duplicity/credentials
duplicity --exclude '/home/kev/.*' \
          --encrypt-sign-key="$GPG_KEY" \
          --log-file "/home/kev/.duplicity/info.log" \
          /home/kev/ pydrive+gdocs://lyterk@gmail.com/backup
