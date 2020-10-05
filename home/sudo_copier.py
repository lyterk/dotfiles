#!/usr/bin/env python3

from pathlib import Path

home = Path("/home/lyterk")

dotfiles_path = home / "dotfiles" / "home"

files = {
        "systemd/logind.conf": Path("/etc/systemd/logind.conf"),
        "sudo/sshd_config": Path("/etc/ssh/sshd_config")
}

for origin, destination in files.items():
    print(destination)
    try:
        (dotfiles_path / origin).replace(destination)
    except Exception as exc:
        print(exc)
