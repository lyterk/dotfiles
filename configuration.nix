# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <home-manager/nixos>
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "laptop"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
  ];

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

  # Configure console keymap
  console.keyMap = "dvorak";

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    # vpn configuration
    tailscale.enable = true;
    # privacy vpn
    mullvad-vpn.enable = true;
    # For gpg key reading -- smart cards
    pcscd.enable = true;
    # Sound control, better api than pavucontrol. But I still install that anyway
    pipewire = {
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
    resolved = {
      enable = true;
      dnssec = "true";
      domains = [ "~." ];
      fallbackDns = [
        "1.1.1.1#one.one.one.one"
        "1.0.0.1#one.one.one.one"
      ];
      dnsovertls = "true";
    };

  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.polkit.enable = true;
  security.rtkit.enable = true;

  systemd.user.services.kanshi = {
    description = "kanshi daemon";
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kanshi}/bin/kanshi -c kanshi_config_file";
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
    ];
    packages = with pkgs; [
      (pass.withExtensions (ext: [ ext.pass-otp ]))
      rofi-pass-wayland
      rofimoji
      vlc
      thunderbird
    ];
  };

  # home-manager.users.lyterk = import /home/lyterk/.config/home-manager/home.nix;

  programs = {
    # sway.enable = true;
    # Adjusting brightness with keys
    light.enable = true;
    # System-wide I guess?
    firefox.enable = true;
    fish.enable = true;
    steam.enable = true;
  };

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # https://github.com/NixOS/nixpkgs/issues/240886
  security.pam.services.gtklock = { };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # nix specific
    home-manager
    nixfmt-rfc-style
    nix-ld
    # build
    gcc
    # editors
    neovim
    emacs29
    # shells
    fish
    zsh
    # shell utilities
    bat
    eza
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
    # network
    tailscale
    mullvad-vpn
  ];

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

  # Connecting devices via USB to calibre
  # https://nixos.wiki/wiki/Calibre
  services.udisks2.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.iproute2.enable = true; # ostensibly useful for mullvad

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
