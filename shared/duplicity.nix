{
  config,
  lib,
  pkgs,
  ...
}:
let
  homedir = "/home/lyterk";
in
{
  duplicity = {
    enable = true;

    # The root path to back up
    root = homedir;
    # Paths to include (`duplicity` automatically includes all unless exclusions are added)
    include = [
      homedir # Add any specific paths you'd like to include explicitly
    ];

    # Paths to exclude
    exclude = [
      "${homedir}/.cache"
      "${homedir}/.npm"
      "${homedir}/.mozilla"
      "${homedir}/.PlayOnLinux"
      "${homedir}/.cache"
      "${homedir}/.clojure"
      "${homedir}/.config"
      "${homedir}/.factorio"
      "${homedir}/.gitlibs"
      "${homedir}/.git-credential-cache"
      "${homedir}/.gnupg"
      "${homedir}/.google-chrome-captive"
      "${homedir}/.hex"
      "${homedir}/.ipython"
      "${homedir}/.java"
      "${homedir}/.keychain"
      "${homedir}/.local"
      "${homedir}/.m2"
      "${homedir}/.mc"
      "${homedir}/.minio"
      "${homedir}/.mix"
      "${homedir}/.mozilla"
      "${homedir}/.nix-defexpr"
      "${homedir}/.nix-profile"
      "${homedir}/.npm"
      "${homedir}/.password-store"
      "${homedir}/.pki"
      "${homedir}/.sbt"
      "${homedir}/.ssh"
      "${homedir}/.steam"
      "${homedir}/.tenv"
      "${homedir}/.terraform.d"
      "${homedir}/.thunderbird"
      "${homedir}/.tor"
      "${homedir}/.yarn"
    ];

    # Target backup URL (SFTP using your SSH config host `desktop`)
    targetUrl = "rsync://desktop/mnt/orange/backups/oldLenovo";

    # Frequency of backups (daily backups at midnight)
    frequency = "daily";

    # Secret file to hold encryption or access credentials (optional, if encryption is required)
    secretFile = null;

    # Extra flags passed to the `duplicity` command (optional)
    extraFlags = [
      "--volsize"
      "100"
    ]; # Adjust volume size to 100MB chunks for larger backups

    # Perform full backups every 1 month; incremental backups in between
    fullIfOlderThan = "1M";

    # Cleanup policies (optional)
    cleanup = {
      maxAge = "1M"; # Keep backups no older than 6 months
      maxFull = 2; # Retain at least 2 full backups
      maxIncr = 1; # Remove old incremental backups when newer full backups are available
    };
  };
}
