#!/usr/bin/env python3

from pathlib import Path

home = Path.home()
dotfiles_path = home / "dotfiles" / "home"

files = {
    "config.fish": home / ".config/fish/config.fish",
    "systemd/emacs.service": home / ".config/systemd/user/emacs.service",
    "systemd/syncthing.service": home / ".config/systemd/user/syncthing.service",
    "systemd/redshift.service": home / ".config/systemd/user/redshift.service",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "spacemacs": home / ".spacemacs",
    "pip.conf": home / ".pip/pip.conf",
    "kdewalletrc": home / ".config/kdewalletrc",
    "redshift.conf": home / ".config/redshift.conf",
    "flake8": home / ".config/flake8",
}

# Remove existing symlinked destinations
for _, value in files.items():
    print(value)
    try:
        value.unlink()
        value.parent.mkdir(parents=True, exist_ok=True)
    except FileNotFoundError:
        pass
    except Exception as exc:
        print(exc)

# Create new symlink from dotfiles directory
[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
