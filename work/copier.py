#!/usr/bin/env python3

from pathlib import Path

home = Path("~").expanduser()
dotfiles_path = home / "dotfiles" / "work"
snippets_source = home / "dotfiles" / "snippets"
snippets_destination = home / ".emacs.d" / "private" / "snippets"

files = {
    "config": home / ".ssh/config",
    "config.fish": home / ".config/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "aspell.en.pws": home / ".aspell.en.pws",
    "spacemacs.el": home / ".spacemacs",
    "pylintrc": home / ".pylintrc",
    "flake8": home / ".config/flake8",
    "jsbeautifyrc": home / ".jsbeautifyrc",
    "userChrome.css": home
    / ".mozilla/firefox/15naj99r.default-release/chrome/userChrome.css",
    "systemd/emacs.service": home / ".config/systemd/user/emacs.service",
    "systemd/bell.service": home / ".config/systemd/user/bell.service",
    "systemd/offlineimap-oneshot.service": home
    / ".config/systemd/user/offlineimap-oneshot.service",
    "systemd/offlineimap-oneshot.timer": home
    / ".config/systemd/user/offlineimap-oneshot.timer",
    "systemd/notmuch.service": home / ".config/systemd/user/notmuch.service",
    "systemd/notmuch.timer": home / ".config/systemd/user/notmuch.timer",
    "offlineimaprc": home / ".offlineimaprc",
}

for dir in snippets_source.iterdir():
    Path(snippets_destination / dir.name).mkdir(parents=False, exist_ok=True)
    for snippet in dir.iterdir():
        try:
            (snippets_destination / dir.name / snippet.name).symlink_to(
                snippets_source / dir.name / snippet.name
            )
        except:
            (snippets_destination / dir.name / snippet.name).unlink()
            (snippets_destination / dir.name / snippet.name).symlink_to(
                snippets_source / dir.name / snippet.name
            )


# Remove existing symlinked destinations
for _, value in files.items():
    print(value)
    try:
        value.unlink()
        value.parent.mkdir(parents=True, exist_ok=True)
    except Exception as exc:
        print(exc)
        pass

# Create new symlink from dotfiles directory
[value.symlink_to(dotfiles_path / key) for key, value in files.items()]
