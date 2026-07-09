{
  lib,
  pkgs,
  ...
}:

let
  commonServiceConfig = {
    after = [
      "network-online.target"
      "sops-nix.service"
    ];
    wants = [ "network-online.target" ];
    requires = [ "sops-nix.service" ];
    wantedBy = [ "multi-user.target" ];
  };

  mkGpgService =
    { name, ... }:
    keyFilename: {
      name = "deploy-gpg-${keyFilename}-${name}";
      value = commonServiceConfig // {
        description = "Import ${keyFilename} GPG key for ${name}";
        serviceConfig = {
          Type = "oneshot";
          User = name;
          Group = "users";
          RemainAfterExit = true;
          ExecStart = "${pkgs.gnupg}/bin/gpg --import /run/secrets/${keyFilename}";
        };
      };
    };
in
{
  options.sopsFunctions = {
    mkGpgService = lib.mkOption {
      type = lib.types.raw;
      default = mkGpgService;
      readOnly = true;
    };
  };
}
