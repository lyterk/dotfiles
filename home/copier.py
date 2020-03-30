#!/usr/bin/env python3

from pathlib import Path

home = Path.home()
dotfiles_path = home / ".dotfiles" / "home"


def firefox_settings():
    import configparser

    firefox_ini = str(home / ".mozilla" / "firefox" / "profiles.ini")

    profiles = configparser.ConfigParser()
    profiles.read(firefox_ini)

    firefox_profile_paths = []
    for k, v in profiles.items():
        if "Profile" in k:
            firefox_profile_paths.append(v.get("Path"))

    for profile_key, profile in profiles.items():
        if "Profile" in profile_key:
            firefox_profile = (
                home / ".mozilla" / "firefox" / profile.get("Path") / "chrome"
            )
            firefox_profile.mkdir(parents=True, exist_ok=True)

            try:
                (firefox_profile / "userChrome.css").symlink_to(
                    dotfiles_path / "browser" / "userChrome.css"
                )
            except FileExistsError:
                pass

            print(f"Copied Firefox profile {profile.get('Path')}")


doom_source = dotfiles_path / "doom.d"
doom_destination = home / ".doom.d"

files = {
    "spacemacs": home / ".spacemacs",
    "config": home / ".ssh" / "config",
    "config.fish": home / ".config/fish/config.fish",
    "gitconfig": home / ".gitconfig",
    "gitignore": home / ".gitignore",
    "pip.conf": home / ".pip/pip.conf",
    "kdewalletrc": home / ".config/kdewalletrc",
    "redshift.conf": home / ".config/redshift.conf",
    "flake8": home / ".config/flake8",
    # "projectile": home / ".projectile",
    "rustfmt.toml": home / ".config/rustfmt/rustfmt.toml",
    # "browser/userChrome.css": firefox_profile / "userChrome.css",
    "zshenv": home / ".zshenv",
    "zshrc": home / ".zshrc",
    "systemd/emacs.service": home / ".config/systemd/user/emacs.service",
    "systemd/gitwatch@.service": home / ".config/systemd/user/gitwatch@.service",
}

firefox_settings()

for file in doom_source.iterdir():
    try:
        (doom_destination / file.name).symlink_to(doom_source / file.name)
        print(doom_destination / file.name)
    except:
        (doom_destination / file.name).unlink()
        (doom_destination / file.name).symlink_to(doom_source / file.name)
        print(doom_destination / file.name)

# snippets_source = home / "dotfiles" / "snippets"
# snippets_destination = home / ".emacs.d" / "private" / "snippets"

# for dir in snippets_source.iterdir():
#     Path(snippets_destination / dir.name).mkdir(parents=False, exist_ok=True)
#     for snippet in dir.iterdir():
#         try:
#             (snippets_destination / dir.name / snippet.name).symlink_to(
#                 snippets_source / dir.name / snippet.name
#             )
#         except:
#             (snippets_destination / dir.name / snippet.name).unlink()
#             (snippets_destination / dir.name / snippet.name).symlink_to(
#                 snippets_source / dir.name / snippet.name
#             )

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
