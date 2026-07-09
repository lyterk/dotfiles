{
  lib,
  pkgs,
  ...
}:
let
  defaults = {
    volsize = "100";
    fullIfOlderThan = "1M";
    maxFull = "2";
    excludeDirs = [
      ".PlayOnLinux"
      ".android"
      ".cache"
      ".clojure"
      ".config"
      ".dartServer"
      ".dart-tool"
      ".factorio"
      ".git-credential-cache"
      ".gitlibs"
      ".gnome2"
      ".gnupg"
      ".google-chrome-captive"
      ".hex"
      ".ipython"
      ".java"
      ".keychain"
      ".local"
      ".m2"
      ".mc"
      ".minio"
      ".mix"
      ".mozilla"
      ".nix-defexpr"
      ".nix-profile"
      ".npm"
      ".ollama"
      ".password-store"
      ".pki"
      ".sbt"
      ".ssh"
      ".steam"
      ".tenv"
      ".terraform.d"
      ".thunderbird"
      ".tor"
      ".yarn"
      "Downloads"
    ];
  };

  mkExcludeFlags =
    homedir:
    lib.concatMap (d: [
      "--exclude"
      "${homedir}/${d}"
    ]) defaults.excludeDirs;

  mkBackupArgs =
    { homedir, targetUrl }:
    [
      "--volsize"
      defaults.volsize
      "--full-if-older-than"
      defaults.fullIfOlderThan
    ]
    ++ mkExcludeFlags homedir
    ++ [
      homedir
      targetUrl
    ];

  mkDuplicityService =
    {
      user,
      homedir,
      targetUrl,
      secretFile,
      ...
    }:
    {
      description = "Duplicity backup for ${user}";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      unitConfig.ConditionACPower = true;
      serviceConfig = {
        Type = "oneshot";
        User = user;
        Group = "users";
        RuntimeDirectory = "duplicity-${user}";
        ExecStart = lib.escapeShellArgs (
          [
            "${pkgs.util-linux}/bin/flock"
            "--nonblock"
            "/run/duplicity-${user}/lock"
            "${pkgs.duplicity}/bin/duplicity"
          ]
          ++ mkBackupArgs { inherit homedir targetUrl; }
        );
        EnvironmentFile = secretFile;
      };
    };

  mkDuplicityTimer =
    { user, ... }:
    {
      description = "Duplicity backup timer for ${user}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        Persistent = true;
        RandomizedDelaySec = "3h";
        OnCalendar = "*-*-* 05:00:00";
      };
    };

  mkDuplicityCleanupService =
    {
      user,
      targetUrl,
      secretFile,
      ...
    }:
    {
      description = "Duplicity cleanup for ${user}";
      after = [
        "duplicity-${user}.service"
        "network-online.target"
      ];
      requires = [ "duplicity-${user}.service" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "duplicity-${user}.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = user;
        Group = "users";
        RuntimeDirectory = "duplicity-${user}";
        ExecStart = lib.escapeShellArgs [
          "${pkgs.util-linux}/bin/flock"
          "/run/duplicity-${user}/lock"
          "${pkgs.duplicity}/bin/duplicity"
          "remove-all-but-n-full"
          defaults.maxFull
          "--force"
          targetUrl
        ];
        EnvironmentFile = secretFile;
      };
    };

  mkDuplicityUser =
    args@{
      user,
      homedir,
      targetUrl,
      secretFile,
    }:
    {
      services = {
        "duplicity-${user}" = mkDuplicityService args;
        "duplicity-cleanup-${user}" = mkDuplicityCleanupService args;
      };
      timers = {
        "duplicity-${user}" = mkDuplicityTimer args;
      };
    };
in
{
  options.duplicity.mkDuplicityUser = lib.mkOption {
    type = lib.types.raw;
    default = mkDuplicityUser;
    readOnly = true;
    description = ''
      Function that returns { services, timers } attrsets for a given user.
      Merge the results into systemd.services and systemd.timers respectively.
    '';
  };
}
