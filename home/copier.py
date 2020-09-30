table=[["config", "~/.ssh/config", ""], ["gitconfig", "~/.gitconfig", ""], ["gitignore", "~/.gitignore", ""], ["pip.conf", "~/.pip/pip.conf", ""], ["kdewalletrc", "~/.config/kdewalletrc", ""], ["redshift.conf", "~/.config/redshift.conf", ""], ["flake8", "~/.config/flake8", ""], ["rustfmt.toml", "~/.config/rustfmt/rustfmt.toml", ""], ["zshenv", "~/.zshenv", ""], ["zshrc", "~/.zshrc", ""], ["profile", "~/.profile", ""], ["zprofile", "~/.zprofile", ""], ["systemd/emacs.service", "~/.config/systemd/user/emacs.service", ""], ["systemd/gitwatch@.service", "~/.config/systemd/user/gitwatch@.service", ""]]
from pathlib import Path

home = Path.home()
dotfiles_path = home / "dotfiles" / "home"


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

file_mappings = {(dotfiles_path / row[0]): Path(row[1]).expanduser() for row in table }

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
