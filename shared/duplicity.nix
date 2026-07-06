{
  config,
  lib,
  pkgs,
  ...
}:
let
  mkDuplicityConfig =
    {
      homedir,
      targetUrl,
      secretFile ? null,
    }:
    {
      enable = true;
      root = homedir;
      include = [ homedir ];
      exclude = [
        "${homedir}/.cache"
        # ... rest of your excludes
      ];
      targetUrl = targetUrl;
      frequency = "daily";
      secretFile = secretFile;
      extraFlags = [
        "--volsize"
        "100"
      ];
      fullIfOlderThan = "1M";
      cleanup = {
        maxAge = "1M";
        maxFull = 2;
        maxIncr = 1;
      };
    };

  mkExcludeFlags =
    homedir:
    lib.concatMap
      (dir: [
        "--exclude"
        dir
      ])
      [
        "${homedir}/.cache"
        "${homedir}/.npm"
        "${homedir}/.mozilla"
        "${homedir}/.PlayOnLinux"
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
        "${homedir}/.nix-defexpr"
        "${homedir}/.nix-profile"
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

  mkDuplicityService =
    {
      user,
      homedir,
      targetUrl,
      secretFile ? null,
    }:
    {
      description = "Duplicity backup for ${user}";
      startAt = "daily";
      serviceConfig = {
        User = "root";
        ExecStart = lib.concatStringsSep " " (
          [ "${pkgs.duplicity}/bin/duplicity" ]
          ++ [
            "--volsize"
            "100"
          ]
          ++ [
            "--full-if-older-than"
            "1M"
          ]
          ++ (mkExcludeFlags homedir)
          ++ [
            homedir
            targetUrl
          ]
        );
        EnvironmentFile = lib.mkIf (secretFile != null) secretFile;
      };
    };

  mkDuplicityCleanupService =
    {
      user,
      homedir,
      targetUrl,
      secretFile ? null,
    }:
    {
      description = "Duplicity cleanup for ${user}";
      after = [ "duplicity-${user}.service" ];
      requires = [ "duplicity-${user}.service" ];
      wantedBy = [ "duplicity-${user}.service" ];
      serviceConfig = {
        User = "root";
        ExecStart = lib.concatStringsSep " " [
          "${pkgs.duplicity}/bin/duplicity"
          "remove-all-but-n-full"
          "2"
          "--force"
          targetUrl
        ];
        EnvironmentFile = lib.mkIf (secretFile != null) secretFile;
      };
    };

  mkDuplicityUser =
    args@{
      user,
      homedir,
      targetUrl,
      secretFile ? null,
    }:
    {
      "duplicity-${user}" = mkDuplicityService args;
      "duplicity-cleanup-${user}" = mkDuplicityCleanupService args;
    };

in
{
  options = {
    lyterBackups = {
      mkDuplicityConfig = lib.mkOption {
        type = lib.types.raw;
        default = mkDuplicityConfig;
        readOnly = true;
      };

      mkDuplicityUser = lib.mkOption {
        type = lib.types.raw;
        default = mkDuplicityUser;
        readOnly = true;
      };
    };
  };
}
