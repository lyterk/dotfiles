#!/usr/bin/env python3

from pathlib import Path
from typing import Dict

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles" / "work"
all_scripts_path = home / "dotfiles" / "scripts"

snippets_source = home / "dotfiles" / "snippets"
snippets_destination = home / ".emacs.d" / "private" / "snippets"

doom_source = home / "dotfiles" / "work" / "doom.d"
doom_destination = home / ".doom.d"

files = {
    # dotfiles
    "ssh_config": home / ".ssh/config",
    "fish/config.fish": home / ".config/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore_global",
    "aspell.en.pws": home / ".aspell.en.pws",
    "spacemacs.el": home / ".spacemacs",
    "pylintrc": home / ".pylintrc",
    "flake8": home / ".config/flake8",
    "jsbeautifyrc": home / ".jsbeautifyrc",
    "userChrome.css": home
    / ".mozilla/firefox/15naj99r.default-release/chrome/userChrome.css",
    "userChrome.css": home / ".mozilla/firefox/default.default/chrome/userChrome.css",
    "offlineimaprc": home / ".offlineimaprc",
    "zshrc": home / ".zshrc",
    "zshenv": home / ".zshenv",
    # Systemd stuff
    "systemd/emacs.service": home / ".config/systemd/user/emacs.service",
    "systemd/znc.service": home / ".config/systemd/user/znc.service",
    "systemd/bell.service": home / ".config/systemd/user/bell.service",
    "systemd/offlineimap-oneshot.service": home
    / ".config/systemd/user/offlineimap-oneshot.service",
    "systemd/offlineimap-oneshot.timer": home
    / ".config/systemd/user/offlineimap-oneshot.timer",
    "systemd/notmuch.service": home / ".config/systemd/user/notmuch.service",
    "systemd/notmuch.timer": home / ".config/systemd/user/notmuch.timer",
    "systemd/gitwatch@.service": home / ".config/systemd/user/gitwatch@.service",
    "scripts/vpn.sh" : home / ".local/bin/vpn",
    "fish/functions/sudo.fish": home / ".config/fish/functions/sudo.fish"
}

# Remove existing symlinked destinations
def copy(root_path: Path, files: Dict[str, Path]):
    for filename, destination_path in files.items():
        print(destination_path)
        try:
            destination_path.unlink()
        except:
            # I don't care if it already exists or not.
            pass
        try:
            destination_path.parent.mkdir(parents=True, exist_ok=True)
            destination_path.symlink_to(root_path / filename)
        except Exception as exc:
            print(exc)
            pass

copy(dotfiles_path, files)

scripts = {
    "passmenu.sh": home / ".local/bin/passmenu"
}

copy(all_scripts_path, scripts)
