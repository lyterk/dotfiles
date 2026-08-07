{
  lib,
  pkgs,
  ...
}:
let
  defaults = {
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
      ".pub-cache"
      ".rustup"
      ".sbt"
      ".sobelow"
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

  mkExcludeArgs =
    homedir:
    lib.concatMap (d: [
      "--exclude"
      "${homedir}/${d}"
    ]) defaults.excludeDirs;

  mkResticService =
    {
      user,
      homedir,
      targetUrl,
      secretFile,
      ...
    }:
    {
      description = "Restic backup for ${user}";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      unitConfig.ConditionACPower = true;
      serviceConfig = {
        Type = "oneshot";
        User = user;
        Group = "users";
        RuntimeDirectory = "restic-${user}";
        ExecStart = lib.escapeShellArgs (
          [
            "${pkgs.util-linux}/bin/flock"
            "--nonblock"
            "/run/restic-${user}/lock"
            "${pkgs.restic}/bin/restic"
          ]
          ++ mkExcludeArgs homedir
          ++ [
            "--exclude"
            "node_modules"
          ]
          ++ [
            "backup"
            homedir
          ]
        );
        EnvironmentFile = secretFile;
        Environment = [
          "RESTIC_REPOSITORY=${targetUrl}"
        ];
      };
    };

  mkResticTimer =
    { user, ... }:
    {
      description = "Restic backup timer for ${user}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        Persistent = true;
        RandomizedDelaySec = "3h";
        OnCalendar = "*-*-* 05:00:00";
      };
    };

  mkResticForgetService =
    {
      user,
      targetUrl,
      secretFile,
      ...
    }:
    {
      description = "Restic cleanup for ${user}";
      after = [
        "restic-${user}.service"
        "network-online.target"
      ];
      requires = [ "restic-${user}.service" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "restic-${user}.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = user;
        Group = "users";
        RuntimeDirectory = "restic-${user}";
        ExecStart = lib.escapeShellArgs [
          "${pkgs.util-linux}/bin/flock"
          "/run/restic-${user}/lock"
          "${pkgs.restic}/bin/restic"
          "forget"
          "--keep-monthly"
          "3"
          "--prune"
        ];
        EnvironmentFile = secretFile;
        Environment = [
          "RESTIC_REPOSITORY=${targetUrl}"
        ];
      };
    };

  mkResticUser =
    args@{
      user,
      homedir,
      targetUrl,
      secretFile,
    }:
    {
      services = {
        "restic-${user}" = mkResticService args;
        "restic-cleanup-${user}" = mkResticForgetService args;
      };
      timers = {
        "restic-${user}" = mkResticTimer args;
      };
    };
in
{
  options.restic.mkResticUser = lib.mkOption {
    type = lib.types.raw;
    default = mkResticUser;
    readOnly = true;
    description = ''
      Function that returns { services, timers } attrsets for a given user.
      Merge the results into systemd.services and systemd.timers respectively.
    '';
  };
}
