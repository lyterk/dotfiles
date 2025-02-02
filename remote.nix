# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <home-manager/nixos>
  ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  boot.initrd.luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".device = "/dev/disk/by-uuid/fd1d935d-35e5-4b29-99f9-ea309ab83efc";
  # Setup keyfile
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };

  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.luks.devices."luks-1b0c3782-0fa1-4b3e-9588-f44d5564da32".keyFile = "/boot/crypto_keyfile.bin";
  boot.initrd.luks.devices."luks-fd1d935d-35e5-4b29-99f9-ea309ab83efc".keyFile = "/boot/crypto_keyfile.bin";
  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the XFCE Desktop Environment.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.xfce.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
  };

  # Configure console keymap
  console.keyMap = "dvorak";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;

  services.gnome.gnome-keyring.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lyterk = {
    isNormalUser = true;
    description = "Kevin Lyter";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = with pkgs; [ ];
  };

  home-manager.backupFileExtension = "backup";

  home-manager.users.lyterk =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        alacritty
        kdePackages.kdeconnect-kde
      ];
      programs.fish.enable = true;
      home.stateVersion = "24.11";

      services = {
        gpg-agent = {
          enable = true;
          defaultCacheTtl = 3600;
          maxCacheTtl = 86400;
          pinentryPackage = pkgs.pinentry-qt;
        };
      };

      wayland.windowManager.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        config = rec {
          modifier = "Mod4";
          terminal = "alacritty";
        };
      };
    };

  programs = {
    light.enable = true;
    firefox.enable = true;

    fish = {
      enable = true;

      shellAliases = {
        vim = "nvim";
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
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages =
    with pkgs;
    [
      #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      #  wget
      #  thunderbird
      # shell utilities
      fish
      zsh
      fd
      git
      jq
      xsv
      rlwrap
      htop
      unzip
      ripgrep
      tree
      wget
      neovim
      emacs
      (pass.withExtensions (ext: [ ext.pass-otp ]))
      rofi-pass-wayland
      gnupg
      pinentry-qt
      tailscale
      # nix stuff
      nil
      nixfmt-rfc-style
      nix-ld
      # sway stuff
      ## screenshots
      grim
      slurp
      ## wl-copy/wl-paste
      wl-clipboard
      ## notifications
      mako
    ]
    ++ shellUtilities;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services = {
    openssh = {
      enable = true;
      ports = [ 55555 ];
      settings = {
        PasswordAuthentication = false;
        AllowUsers = [ "lyterk" ];
        X11Forwarding = false;
        PermitRootLogin = "no";
      };
    };
    tailscale.enable = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  # networking.firewall = rec {
  #   allowedTCPPortRanges = [
  #     { from = 1714; to = 1764; }
  #   ];
  #   allowedUDPPortRanges = allowedTCPPortRanges;
  # };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
