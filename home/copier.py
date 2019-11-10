#!/usr/bin/env python3

from pathlib import Path
import configparser

home = Path.home()
dotfiles_path = home / "dotfiles" / "home"

firefox_ini = str(home / ".mozilla" / "firefox" / "profiles.ini")
profile = configparser.ConfigParser()
profile.read(firefox_ini)
# This file will always point to the 0th profile of the user, if Firefox is installed
firefox_profile = (
    home / ".mozilla" / "firefox" / profile.get("Profile0", "Path") / "chrome"
)

files = {
    "spacemacs": home / ".spacemacs",
    "config": home / ".ssh" / "config",
    "config.fish": home / ".config/fish/config.fish",
    "systemd/emacs.service": home / ".config/systemd/user/emacs.service",
    "systemd/redshift.service": home / ".config/systemd/user/redshift.service",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "pip.conf": home / ".pip/pip.conf",
    "kdewalletrc": home / ".config/kdewalletrc",
    "redshift.conf": home / ".config/redshift.conf",
    "flake8": home / ".config/flake8",
    "projectile": home / ".projectile",
    "browser/userChrome.css": firefox_profile / "userChrome.css",
}

snippets_source = home / "dotfiles" / "snippets"
snippets_destination = home / ".emacs.d" / "private" / "snippets"

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
