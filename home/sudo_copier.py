#!/usr/bin/env python3

from pathlib import Path as P

home = P("/home/lyterk")

dotfiles_path = home / "dotfiles/home"

files = {dotfiles_path / "sudo/fishlogin": P("/usr/local/bin/fishlogin")}

for origin, destination in files.items():
    print(destination)
    try:
        (dotfiles_path / origin).replace(destination)
    except Exception as exc:
        print(exc)
