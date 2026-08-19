# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  ...
}:

let
  users = [
    {
      name = "lyterk";
      homedir = "/home/lyterk";
      targetUrl = "";
      secretFile = "/run/secrets/resticSecrets";
      gpgKeys = [
        "gpgCode"
        "gpgKev"
      ];
    }
    {
      name = "work";
      homedir = "/home/work";
      targetUrl = "";
      secretFile = "/run/secrets/resticSecrets";
      gpgKeys = [
        "gpgCode"
        "gpgKev"
      ];
    }
  ];

  resticUnits = map (
    u:
    config.restic.mkResticUser {
      user = u.name;
      homedir = u.homedir;
      targetUrl = "s3:s3.us-west-2.amazonaws.com/lyterk-backups-383137109783-us-west-2-an/hosts/miranda/users/${u.name}";
      secretFile = u.secretFile;
    }
  ) users;

in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware.nix
    ../../shared/configuration-base.nix
  ];

  boot = {
    # Bootloader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [ "mem_sleep_default=deep" ];
    # </ Bootloader>
    initrd.systemd.enable = true;
  };

  networking = {
    hostName = "miranda";
  };

  # Configure console keymap
  # console.keyMap = "dvorak";
  console.keyMap = "us";

  services = {
    bell = {
      enable = true;
      user = "lyterk";
      intervalMinutes = 33;
    };
  };

  systemd = {
    services = lib.mkMerge ((map (x: x.services) resticUnits));

    user.services = {
      kanshi = {
        description = "kanshi daemon";
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.kanshi}/bin/kanshi -c kanshi_config_file";
        };
      };

      emacs.serviceConfig = {
        Slice = "session.slice";
      };
    };
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lyterk = {
    shell = pkgs.fish;
    isNormalUser = true;
    description = "Kevin Lyter";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "docker"
    ];
  };

  users.users.work = {
    shell = pkgs.fish;
    isNormalUser = true;
    description = "Kevin Lyter (Yotta)";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "docker"
    ];
  };

  home-manager.users.work = ../../shared/users/work-home.nix;
  # home-manager.users.lyterk = ../../shared/users/lyterk-home.nix;

  # Building raspberry pis
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
