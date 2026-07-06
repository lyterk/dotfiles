{
  lib,
  pkgs,
  ...
}:

let
  mkDeploySshScript =
    { user, homedir }:
    pkgs.writeShellApplication {
      name = "deploy-ssh-key-${user}";
      runtimeInputs = [ pkgs.coreutils ];
      text = ''
        mkdir -p ${homedir}/.ssh
        rm -f ${homedir}/.ssh/id_ed25519
        ln -sf /run/secrets/sshPrivateKey ${homedir}/.ssh/id_ed25519
        chmod 0600 ${homedir}/.ssh/id_ed25519
        chown ${user}:users ${homedir}/.ssh/id_ed25519
      '';
    };

  mkGpgKeyScript =
    { user, keyFilename }:
    pkgs.writeShellApplication {
      name = "deploy-gpg-key-${user}-${keyFilename}";
      runtimeInputs = [
        pkgs.gnupg
        pkgs.coreutils
      ];
      text = ''
        ${pkgs.gnupg}/bin/gpg --import /run/secrets/${keyFilename}
        chmod 0700 ~/.gnupg
      '';
    };
  mkSshService =
    { name, homedir, ... }:
    {
      name = "deploy-ssh-key-${name}";
      value = {
        description = "Deploy SSH private key for ${name}";
        after = [
          "network-online.target"
          "sops-nix.service"
        ];
        wants = [ "network-online.target" ];
        requires = [ "sops-nix.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${
            mkDeploySshScript {
              user = name;
              inherit homedir;
            }
          }/bin/deploy-ssh-key-${name}";
          User = name;
          Group = "users";
          RemainAfterExit = true;
        };
      };
    };

  mkGpgService =
    { name, ... }:
    keyFilename: {
      name = "deploy-gpg-${keyFilename}-${name}";
      value = {
        description = "Deploy ${keyFilename} GPG key for ${name}";
        after = [
          "network-online.target"
          "sops-nix.service"
        ];
        wants = [ "network-online.target" ];
        requires = [ "sops-nix.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${
            mkGpgKeyScript {
              user = name;
              inherit keyFilename;
            }
          }/bin/deploy-gpg-key-${name}-${keyFilename}";
          User = name;
          Group = "users";
          RemainAfterExit = true;
        };
      };
    };
in
{

  options = {
    sopsFunctions = {
      mkSshService = lib.mkOption {
        type = lib.types.raw;
        default = mkSshService;
        readOnly = true;
      };
      mkGpgService = lib.mkOption {
        type = lib.types.raw;
        default = mkGpgService;
        readOnly = true;
      };
    };
  };
}
