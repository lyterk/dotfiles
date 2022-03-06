from pathlib import Path

home = Path.home()
dotfiles_path = home / "dotfiles" / "home"

dotfile_entries = [
    ["ssh_config", "~/.ssh/config"],
    ["gitconfig", "~/.gitconfig"],
    ["gitignore", "~/.gitignore"],
    ["i3_config", "~/.config/i3/config"],
    ["pip.conf", "~/.pip/pip.conf"],
    ["kdewalletrc", "~/.config/kdewalletrc"],
    ["redshift.conf", "~/.config/redshift.conf"],
    ["flake8", "~/.config/flake8"],
    ["rustfmt.toml", "~/.config/rustfmt/rustfmt.toml"],
    ["zshenv", "~/.zshenv"],
    ["zshrc", "~/.zshrc"],
    ["profile", "~/.profile"],
    ["fish/config.fish", "~/.config/fish/config.fish"],
    ["mimeapps.list", "~/.local/share/applications/mimeapps.list"],
    ["zprofile", "~/.zprofile", ""],
    ["rofi_config", "~/.config/rofi/config.rasi"],
    ["xfce_terminalrc", "~/.config/xfce4/terminal/terminalrc"],
    ["mbsyncrc.conf", "~/.mbsyncrc"],
    ["tridactylrc", "~/.config/tridactyl/tridactylrc"],
    ["notmuch.conf", "~/.notmuch_config"],
    ["systemd/gitwatch@.service", "~/.local/share/systemd/user/gitwatch@.service"],
    [
        "systemd/backup-oneshot.service",
        "~/.local/share/systemd/user/backup-oneshot.service",
    ],
    ["i3status-rust.toml", "~/.config/i3status-rust/config.toml"],
    ["systemd/backup.timer", "~/.local/share/systemd/user/backup.timer"],
    ["systemd/emacs.service", "~/.local/share/systemd/user/emacs.service"],
]

full_paths = [["~/code/smarthome/sounds/target/release/sounds", "~/.local/bin/sounds"]]


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


firefox_settings()

file_mappings = {
    (dotfiles_path / row[0]): Path(row[1]).expanduser() for row in dotfile_entries
}

for source, target in file_mappings.items():
    print(target)
    try:
        try:
            target.unlink()
        except:
            pass
        target.parent.mkdir(parents=True, exist_ok=True)
        target.symlink_to(source)
    except Exception as exc:
        print(exc)

for func in (dotfiles_path / "fish/functions").glob("*"):
    target = home / ".config/fish/functions" / func.name
    try:
        target.unlink()
    except:
        pass
    target.parent.mkdir(parents=True, exist_ok=True)
    target.symlink_to(func)
