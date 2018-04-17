#!/usr/bin/env python3

from pathlib import Path

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles" / "work"

files = {
    "config": home / ".ssh/config",
    "config.fish": home / ".config/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "spacemacs": home / ".spacemacs",
    "aspell.en.pws": home / ".aspell.en.pws",
    "pylintrc": home / ".pylintrc"
}

# Remove existing symlinked destinations
for _, value in files.items():
    print(value)
    try:
        value.unlink()
        value.parent.mkdir(parents=True)
    except Exception as exc:
        print(exc)
        pass

# Create new symlink from dotfiles directory
[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
