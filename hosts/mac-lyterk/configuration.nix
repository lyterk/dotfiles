{ config, pkgs, ... }:

{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  # System-wide settings
  environment.systemPackages = with pkgs; [
    cargo
    direnv
    fd
    gcc
    git
    git
    gnupg
    neovim
    nil
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    ripgrep
    rust-analyzer
    wget
    zstd
  ];

  environment.variables.HOMEBREW_NO_ANALYTICS = "1";

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };

    brews = [
      "coreutils"
      "trash"
      "pinentry-mac"
    ];

    # Update these applicatons manually.
    # As brew would update them by unninstalling and installing the newest
    # version, it could lead to data loss.
    casks = [
      "emacs-mac" # Emacs fork with better macOS support
      "firefox"
    ];

    taps = [
      "railwaycat/emacsmacport" # emacs-mac
    ];

    masApps = {
      Tailscale = 1475387142; # App Store URL id
    };
  };

  # Set your preferred shell
  programs.fish.enable = true;

  # Set your hostname (change as needed)
  networking.hostName = "mac-lyterk";

  # Enable TouchID for sudo (optional, on supported hardware)
  security.pam.services.sudo_local.touchIdAuth = true;

  # Basic system preferences
  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.stateVersion = 6;
  system.primaryUser = "lyterk";

  # Allow unfree packages (e.g., Google Chrome)
  nixpkgs.config.allowUnfree = true;
}
