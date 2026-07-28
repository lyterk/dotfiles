# Edit this configuration file to define what should be installed on your system.
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
  ];

  # TODO Move to shared
  yaziFilePicker = pkgs.writeShellScriptBin "yazi-filepicker.sh" ''
    echo "Called with: $@" >> /tmp/yazi-picker.log
    ${pkgs.foot}/bin/foot -e ${pkgs.yazi}/bin/yazi  --chooser-file="$5"
  '';

  # TODO Move to shared
  refreshConfigScript = pkgs.writeShellScriptBin "refreshNix.sh" ''
      SUDO_PASSWORD=$(rofi -dmenu -password -no-fixed-num-lines -p "[sudo] password for $USER: ")
      REBUILD_COMMAND="echo \"$SUDO_PASSWORD\" | sudo -S ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch --flake /etc/nixos#miranda"
      if eval "$REBUILD_COMMAND"; then
      ${pkgs.libnotify}/bin/notify-send \
        --urgency=normal \
        --icon=emblem-default \
        "NixOS Rebuild" \
        "Configuration switched successfully ✓"
    else
      ${pkgs.libnotify}/bin/notify-send \
        --urgency=critical \
        --icon=dialog-error \
        "NixOS Rebuild" \
        "Rebuild failed ✗"
    fi
  '';
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
    ./hardware.nix
    ../../shared/sops.nix
    ../../shared/restic.nix
  ];

  # Bootloader.
  boot = {
    loader.grub = {
      enable = true;
      device = "/dev/sda";
      useOSProber = true;
      enableCryptodisk = true;
    };
    initrd = {
      luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".device =
        "/dev/disk/by-uuid/fd1d935d-35e5-4b29-99f9-ea309ab83efc";
      secrets = {
        "/boot/crypto_keyfile.bin" = null;
      };
      luks.devices."luks-1b0c3782-0fa1-4b3e-9588-f44d5564da32".keyFile = "/boot/crypto_keyfile.bin";
      luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".keyFile = "/boot/crypto_keyfile.bin";
    };
  };

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

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd sway";
          user = "lyterk";
        };
      };
    };

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
