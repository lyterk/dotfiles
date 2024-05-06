{ config, pkgs, ... }:

let
  androidSdkModule = import ((builtins.fetchGit {
    url = "https://github.com/tadfisher/android-nixpkgs.git";
    rev = "2d8181caef279f19c4a33dc694723f89ffc195d4"; # on main
    # ref = "main";  # Or "stable", "beta", "preview", "canary"
  }) + "/hm-module.nix");
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "lyterk";
  home.homeDirectory = "/home/lyterk";

  # nix = {
  #   settings.experimental-features = ["nix-command" "flakes"];
  # }


  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # Browsers
    pkgs.firefox
    pkgs.eww
    # Chat
    pkgs.signal-desktop
    # Backups
    # pkgs.deja-dup # TODO https://github.com/NixOS/nixpkgs/issues/122671
    # Editors
    pkgs.emacs
    pkgs.neovim
    # Code
    pkgs.git
    # Shell utilities
    pkgs.eza
    pkgs.ripgrep
    pkgs.wl-clipboard
    pkgs.fish
    pkgs.zsh # it's just so compliant 🥵
    pkgs.fd
    pkgs.atuin
    pkgs.alacritty
    pkgs.htop
    pkgs.keychain
    pkgs.tree
    # Sway utilities
    pkgs.swaylock
    pkgs.swayidle
    pkgs.wob
    pkgs.wev # wayland event viewer
    # Sound utilities
    pkgs.pamixer
    pkgs.pulseaudio
    # Notifications
    pkgs.mako
    # Rofi
    pkgs.rofi
    pkgs.pinentry-rofi
    pkgs.rofi-pass-wayland
    pkgs.rofimoji
    # Secrets
    (pkgs.pass.withExtensions (ext: [ext.pass-otp]))
    # UI dev
    pkgs.nodejs_21
    # Media
    # pkgs.calibre
    pkgs.vlc
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".gitconfig".source = common/gitconfig;
    ".gitignore".source = common/gitignore;
    ".pip/pip.conf".source = common/pip.conf;
    ".config/flake8".source = common/flake8;
    ".config/rustfmt/rustfmt.toml".source = common/rustfmt.toml;
    ".zshenv".source = common/zshenv;
    ".zshrc".source = common/zshrc;
    ".sbclrc".source = common/sbclrc;
    ".config/fish/config.fish".source = common/fish/config.fish;
    ".config/fish/functions/ssh_agent.fish".source = common/fish/functions/ssh_agent.fish;
    ".config/mimeapps.list".source = common/mimeapps.list;
    ".config/rofi/config.rasi".source = common/rofi_config;
    ".config/alacritty/alacritty_base.toml".text = ''
      shell = "/usr/bin/fish"

      [window]
      opacity = 0.9
    '';
    # # Switch between profiles for alacritty
    # ".config/alacritty/circadian.toml".source = common/terminal/circadian.toml;
    # ".config/alacritty/ayu_dark.toml".source = common/terminal/ayu_dark.toml;
    # ".config/alacritty/solarized_light.toml".source = common/terminal/solarized_light.toml;
    # # Systemd
    # ".config/systemd/user/emacs.service".source = systemd/emacs.service;
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/lyterk/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacsclient -t";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # # -------------- Android SDK -------------- #
  # # Android SDK provision, see let binding
  # imports = [ androidSdkModule ];

  # android-sdk.enable = true;

  # # Optional; default path is "~/.local/share/android".
  # # android-sdk.path = "${config.home.homeDirectory}/.android/sdk";

  # android-sdk.packages = sdkPkgs: with sdkPkgs; [
  #   build-tools-34-0-0
  #   cmdline-tools-latest
  #   emulator
  #   platforms-android-34
  #   sources-android-34
  # ];
}
