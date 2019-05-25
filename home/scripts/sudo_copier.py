#!/usr/bin/env python3

from pathlib import Path as P

home = P.home()
dotfiles_path = home / "dotfiles" / "home" / "sudo"
vpn_source = home / "dotfiles" / "vpn"
vpn_destination = P("/etc/openvpn/client")

files = {"hosts": P("/etc/hosts"), "keyboard": P("/etc/default/keyboard")}


# Remove existing symlinked destinations
for key, value in files.items():
    print(value)
    try:
        try:
            value.unlink()
        except Exception as exc:
            print(exc)
        value.parent.mkdir(parents=True, exist_ok=True)
        value.symlink_to(dotfiles_path / key)
    except Exception as exc:
        print(exc)


vpn_destination.mkdir(parents=True, exist_ok=True)
for file_path in vpn_source.iterdir():
    try:
        try:
            # print(vpn_destination / file_path.name)
            (vpn_destination / file_path.name).unlink()
        except Exception as exc:
            # print(exc)
        (vpn_destination / file_path.name).symlink_to(file_path)
    except Exception as exc:
        print(exc)
