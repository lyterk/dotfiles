#!/usr/bin/env sh

rsync -avz ~/dotfiles/hosting/atuin-docker-compose.yaml nuc:~/.local/share/atuin/
rsync -avz ~/dotfiles/common/fish/config.fish nuc:~/.config/fish/config.fish
