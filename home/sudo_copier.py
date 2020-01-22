#!/usr/bin/env python3

from pathlib import Path

home = Path.home()

dotfiles_path = home / "dotfiles" / "home"

files = {"scripts/gitwatch.sh": Path("/") / "usr" / "local" / "bin" / "gitwatch"}

for name, value in files.items():
    print(value)
    try:
        try:
            value.unlink()
        except:
            pass
        value.parent.mkdir(parents=True, exist_ok=True)
        value.symlink_to(dotfiles_path / name)
    except Exception as exc:
        print(exc)
