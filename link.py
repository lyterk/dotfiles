from pathlib import Path
from typing import Dict, List

dotfiles = {
    "ssh_config": "~/.ssh/config",
    "gitconfig": "~/.gitconfig",
    "gitignore": "~/.gitignore",
    "i3_config": "~/.config/i3/config",
    "pip": "~/.pip/pip.conf",
    "kdewalletrc": "~/.config/kdewalletrc",
    "redshift": "~/.config/redshift.conf",
    "flake8": "~/.config/flake8",
    "rustfmt": "~/.config/rustfmt/rustfmt.toml",
    "zshenv": "~/.zshenv",
    "zshrc": "~/.zshrc",
    "sbclrc": "~/.sbclrc",
    "profile": "~/.profile",
    "fish": "~/.config/fish/config.fish",
    "mimeapps": "~/.local/share/applications/mimeapps.list",
    "zprofile": "~/.zprofile",
    "rofi_config": "~/.config/rofi/config.rasi",
    "xfce_terminalrc": "~/.config/xfce4/terminal/terminalrc",
    "mbsyncrc.conf": "~/.mbsyncrc",
    "tridactylrc": "~/.config/tridactyl/tridactylrc",
    "notmuch.conf": "~/.notmuch_config",
    "i3status": "~/.config/i3status-rust/config.toml",
}


systemd = {
    "gitwatch@": "~/.config/systemd/user/gitwatch@.service",
    "backup": "~/.config/systemd/user/backup-oneshot.service",
    "backup": "~/.config/systemd/user/backup.timer",
    "gmail": "~/.config/systemd/user/gmail.timer",
    "fastmail": "~/.config/systemd/user/fastmail.timer",
    "checkmail@": "~/.config/systemd/user/checkmail@.service",
    "emacs": "~/.config/systemd/user/emacs.service",
}

home = Path.home()
dotfiles_path = home / "dotfiles"


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

            source = dotfiles_path / "browser" / "userChrome.css"
            destination = firefox_profile / "userChrome.css"
            try:
                destination.symlink_to(source)
            except FileExistsError:
                pass

            print(f"firefox: {source} -> {destination}")


firefox_settings()


def link_files(directory: str, mappings: Dict[str, str]):
    file_mappings = {
        (dotfiles_path / directory / key): Path(value).expanduser()
        for key, value in mappings.items()
    }

    for source, destination in file_mappings.items():
        print(f"{directory}: {source} -> {destination}")
        try:
            try:
                destination.unlink()
            except:
                pass
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.symlink_to(source)
        except Exception as exc:
            print(exc)


link_files("common", dotfiles)
link_files("systemd", systemd)

for func in (dotfiles_path / "common/fish/functions").glob("*"):
    target = home / ".config/fish/functions" / func.name
    try:
        target.unlink()
    except:
        pass
    target.parent.mkdir(parents=True, exist_ok=True)
    target.symlink_to(func)
