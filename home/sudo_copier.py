#!/usr/bin/env python3

from pathlib import Path as P

home = P("/home/lyterk")

dotfiles_path = home / "dotfiles/home"

files = {
    P("/usr/bin/xdg-open"): P("/usr/local/bin/xoxo"),
    dotfiles_path / "sudo/logind.conf": P("/etc/systemd/logind.conf"),
    dotfiles_path / "sudo/sshd_config": P("/etc/ssh/sshd_config")
}

for origin, destination in files.items():
    print(destination)
    try:
        (dotfiles_path / origin).replace(destination)
    except Exception as exc:
        print(exc)
