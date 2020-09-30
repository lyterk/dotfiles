#!/usr/bin/env python3

from pathlib import Path

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles" / "work"

snippets_source = home / "dotfiles" / "snippets"
snippets_destination = home / ".emacs.d" / "private" / "snippets"

doom_source = home / "dotfiles" / "work" / "doom.d"
doom_destination = home / ".doom.d"

files = {
    # dotfiles
    "config": home / ".ssh/config",
    "config.fish": home / ".config/fish/config.fish",
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
    "scripts/vpn.sh" : home / ".local/bin/vpn"
    "fish/functions/sudo.fish": home / ".config/fish/functions/sudo.fish"
}

# Remove existing symlinked destinations
for filename, destination_path in files.items():
    print(destination_path)
    try:
        destination_path.unlink()
    except:
        # I don't care if it already exists or not.
        pass
    try:
        destination_path.parent.mkdir(parents=True, exist_ok=True)
        destination_path.symlink_to(dotfiles_path / filename)
    except Exception as exc:
        print(exc)
        pass
