from pathlib import Path
from typing import Dict
from subprocess import run


def ok(message: str):
    style, fg, bg = 1, 37, 42
    fmt = ";".join([str(i) for i in [style, fg, bg]])
    s = f"\x1b[{fmt}m {message} \x1b[0m"
    print(s)


def error(message: str):
    style, fg, bg = 1, 37, 41
    fmt = ";".join([str(i) for i in [style, fg, bg]])
    s = f"\x1b[{fmt}m {message} \x1b[0m"
    print(s)


dotfiles = {
    "ssh_config": "~/.ssh/config",
    "gitconfig": "~/.gitconfig",
    "gitignore": "~/.gitignore",
    "i3_config": "~/.config/i3/config",
    "pip": "~/.pip/pip.conf",
    "redshift.conf": "~/.config/redshift.conf",
    "flake8": "~/.config/flake8",
    "rustfmt.toml": "~/.config/rustfmt/rustfmt.toml",
    "gpg-agent.conf": "~/.gnupg/gpg-agent.conf",
    "zshenv": "~/.zshenv",
    "zshrc": "~/.zshrc",
    "sbclrc": "~/.sbclrc",
    "profile": "~/.profile",
    "fish/config.fish": "~/.config/fish/config.fish",
    "fish/functions/ssh_agent.fish": "~/.config/fish/functions/ssh_agent.fish",
    "fish/functions/kchain.fish": "~/.config/fish/functions/kchain.fish",
    "mimeapps.list": "~/.config/mimeapps.list",
    "zprofile": "~/.zprofile",
    "rofi_config": "~/.config/rofi/config.rasi",
    "terminal/xfce_terminalrc": "~/.config/xfce4/terminal/terminalrc",
    "mbsyncrc.conf": "~/.mbsyncrc",
    "tridactylrc": "~/.config/tridactyl/tridactylrc",
    "notmuch.conf": "~/.notmuch_config",
    "i3status-rust.toml": "~/.config/i3status-rust/config.toml",
    "i3-scrot.conf": "~/.config/i3-scrot.conf",
    "emacs-profiles.el": "~/.emacs-profiles.el",
    "dunstrc": "~/.config/dunst/dunstrc",
    "terminal/alacritty.toml": "~/.config/alacritty/alacritty_base.toml",
    "terminal/circadian.toml": "~/.config/alacritty/circadian.toml",
    "terminal/ayu_dark.toml": "~/.config/alacritty/themes/ayu_dark.toml",
    "terminal/solarized_light.toml": "~/.config/alacritty/themes/solarized_light.toml",
}


systemd = {
    "gitwatch@.service": "~/.config/systemd/user/gitwatch@.service",
    "backup-oneshot.service": "~/.config/systemd/user/backup-oneshot.service",
    "backup.timer": "~/.config/systemd/user/backup.timer",
    "gmail.timer": "~/.config/systemd/user/gmail.timer",
    "fastmail.timer": "~/.config/systemd/user/fastmail.timer",
    "checkmail@.service": "~/.config/systemd/user/checkmail@.service",
    "emacs.service": "~/.config/systemd/user/emacs.service",
    "redshift.service": "~/.config/systemd/user/redshift.service",
    "alacritty-circadian.service": "~/.config/systemd/user/alacritty-circadian.service",
}

displays = {
    "laptop.sh": "~/.screenlayout/laptop.sh",
    "monitor.sh": "~/.screenlayout/monitor.sh",
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

            ok(f"firefox: {source} -> {destination}")


def link_files(directory: str, mappings: Dict[str, str]):
    file_mappings = {
        (dotfiles_path / directory / key): Path(value).expanduser()
        for key, value in mappings.items()
    }

    for source, destination in file_mappings.items():
        ok(f"{directory}: {source} -> {destination}")
        try:
            try:
                destination.unlink()
            except:
                pass
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.symlink_to(source)
        except Exception as e:
            error(str(e))


def fish_functions():
    for func in (dotfiles_path / "common/fish/functions").glob("*"):
        target = home / ".config/fish/functions" / func.name
        try:
            target.unlink()
        except Exception as e:
            error(str(e))
        target.parent.mkdir(parents=True, exist_ok=True)
        ok(f"fish: {func} -> {target}")
        target.symlink_to(func)


def gconf_set(settings_file: str):
    target = dotfiles_path / settings_file
    with open(target, "r") as f:
        lines = f.readlines()
    for line in lines:
        args = line.replace("__", " ").split()
        command = " ".join(["gsettings", "set", *args])
        output = run(command, capture_output=True, shell=True)
        if output.stderr:
            error(str(output.stderr))
        else:
            ok(str(command))


if __name__ == "__main__":
    link_files("common", dotfiles)
    link_files("systemd", systemd)
    link_files("displays", displays)
    fish_functions()
    firefox_settings()
    gconf_set("common/gconf/deja-dup.txt")
