# Edit this configuration file to define what should be installed on your system.
{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./hardware.nix
    <home-manager/nixos>
  ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  boot.initrd.luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".device =
    "/dev/disk/by-uuid/fd1d935d-35e5-4b29-99f9-ea309ab83efc";
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };
  boot.loader.grub.enableCryptodisk = true;
  boot.initrd.luks.devices."luks-1b0c3782-0fa1-4b3e-9588-f44d5564da32".keyFile =
    "/boot/crypto_keyfile.bin";
  boot.initrd.luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".keyFile =
    "/boot/crypto_keyfile.bin";

  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  console.keyMap = "dvorak";

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;

  users = {
    users = {
      lyterk = {
        isNormalUser = true;
        description = "Kevin Lyter";
        extraGroups = [
          "networkmanager"
          "wheel"
        ];
        packages = with pkgs; [ ];
        shell = pkgs.fish;
      };
      git = {
        isSystemUser = true;
        group = "git";
        home = "/var/lib/git-server";
        createHome = true;
        shell = "${pkgs.git}/bin/git-shell";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBOdIT7nVDhMpH3CBlGKbyl6YR86IpRd5qRzN1gVnJ5s lyterk@nuc"
        ];
      };
    };
    groups.git = { };
  };

  home-manager.backupFileExtension = "backup";
  home-manager.users.lyterk = ./home.nix;

  programs = {
    light.enable = true;

    fish = {
      enable = true;
      shellAliases = {
        vim = "nvim";
        ls = "exa";
      };
    };

    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
  };

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    fish
    zsh
    fd
    eza
    git
    jq
    rlwrap
    htop
    unzip
    ripgrep
    tree
    wget
    neovim
    emacs
    alacritty
    atuin
    pwgen
    keychain
    plex
    beancount-language-server
    cargo
    clojure
    python3
    poetry
    elixir
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    rofi-pass-wayland
    gnupg
    pinentry-qt
    tailscale
    nil
    direnv
    nixfmt-rfc-style
    nix-ld
    grim
    slurp
    wl-clipboard
    mako
    syncthing
    immich
  ];

  security.pam.services.gtklock = { };

  services = {
    gnome.gnome-keyring.enable = true;

    immich = {
      enable = true;
      port = 2283;
      mediaLocation = "/mnt/orange/immich";
    };

    nginx = {
      enable = true;
      virtualHosts = {
        "localhost" = {
          locations."/" = {
            proxyPass = "http://[::1]:2283";
            proxyWebsockets = true;
            recommendedProxySettings = true;
            extraConfig = ''
              client_max_body_size 50000M;
              proxy_read_timeout   600s;
              proxy_send_timeout   600s;
              send_timeout         600s;
            '';
          };
        };
      };
    };

    plex = {
      enable = true;
      openFirewall = true;
    };

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      settings = {
        devices = { };
        gui = {
          user = "lyterk";
          password = "freddy";
        };
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    printing.enable = true;

    openssh = {
      enable = true;
      ports = [ 65222 ];
      settings = {
        PasswordAuthentication = false;
        AllowUsers = [
          "lyterk"
          "git"
        ];
        X11Forwarding = false;
        PermitRootLogin = "no";
      };
    };

    tailscale.enable = true;
  };

  system.stateVersion = "24.05";
}
