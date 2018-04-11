#!/usr/bin/env python3

from pathlib import Path

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles" / "home"

files = {
    "config.fish": home / ".local/share/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "spacemacs": home / ".spacemacs"
}

# Remove existing symlinked destinations
for _, value in files.items():
    print(value)
    try:
        value.unlink()
    except FileNotFoundError:
        pass

# Create new symlink from dotfiles directory
[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
