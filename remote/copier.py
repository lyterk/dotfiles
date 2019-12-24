#!/usr/bin/env python3

import os
import errno

from pathlib import Path

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles/remote"

files = {
    "config.fish": home / ".config/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "pylintrc": home / ".pylintrc",
    "zshrc": home / ".zshrc",
}


# Create new symlink from dotfiles directory
for _, value in files.items():
    print(value)
    try:
        value.unlink()
        value.parent.mkdir(parents=True, exist_ok=True)
    except Exception as exc:
        print(exc)
        pass

[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
