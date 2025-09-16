{ config, pkgs, ... }:

{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # System-wide settings
  environment.systemPackages = with pkgs; [
    git
    neovim
    wget
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    # Add your favorite packages here!
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
      "direnv"
      "fd"
      "gcc"
      "git"
      "grep"
      "ripgrep"
      "trash"
      "pinentry-mac"
    ];
  
    # Update these applicatons manually.
    # As brew would update them by unninstalling and installing the newest
    # version, it could lead to data loss.
    casks = [
      "emacs-mac" # Emacs fork with better macOS support
      "firefox"
      "alacritty"
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
