#!/usr/bin/env python3

from pathlib import Path

home = Path.home()
dotfiles_path = home / "dotfiles" / "home"

files = {
    "config.fish": home / ".local/share/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "spacemacs": home / ".spacemacs",
    "pip.conf": home / ".pip/pip.conf"
}

# Remove existing symlinked destinations
for _, value in files.items():
    print(value)
    try:
        value.parent.mkdir(parents=True, exist_ok=True)
        value.unlink()
    except FileNotFoundError:
        pass
    except Exception as exc:
        print(exc)

# Create new symlink from dotfiles directory
[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
