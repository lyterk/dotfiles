{ stdenv, config, pkgs, flakeInputs, ... }:

let
  x = 1;
  doomSrc = pkgs.fetchFromGitHub {
    owner = "doomemacs";
    repo = "doomemacs";
    rev = "9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee";
  };
  doomConfigSrc = config.lib.file.mkOutOfStoreSymlink "/home/lyterk/.config/doom";
  doomExecutable = "${doomSrc}/bin/doom";
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "lyterk";
  home.homeDirectory = "/home/lyterk";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  imports = [
    ./nix/batteryNotifier.nix
    # ./nix/emacs.nix
    # flakeInputs.git-doom-emacs
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # pkgs.deja-dup # TODO https://github.com/NixOS/nixpkgs/issues/122671
    # terminal emulators
    alacritty
    atuin
    # fonts
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    noto-fonts-monochrome-emoji
    # file display
    xfce.thunar
    # data stores
    sqlite
    # secrets
    gnupg
    keychain
    # notifications
    mako
    # runtimes
    nodejs_21
    python3
    # sound
    pulseaudio # used for getting and setting the volume
    pamixer
    # interfaces
    wl-clipboard
    rofi
    pinentry-rofi
    pinentry-qt
    wev
    wob
    # monitors
    kanshi
    # network
    mullvad-vpn
    # certs
    # nss-cacert
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
    ".config/sway/config".source = common/sway/sway_config;
    # TODO Figure out the emoji https://github.com/Alexays/Waybar/wiki/Examples
    # ".config/kanshi/config".source = common/sway/kanshi_config;
    ".config/waybar/config".source = common/sway/waybar_config;
    ".config/waybar/style.css".source = common/sway/waybar_style.css;
    ".config/alacritty/alacritty_base.toml".text = ''
      shell = "/usr/bin/env fish"

      [window]
      opacity = 0.9
    '';
    # ".gnupg/gpg-agent.conf".source = common/gpg-agent.conf;
    # # Switch between profiles for alacritty
    # ".config/alacritty/circadian.toml".source = common/terminal/circadian.toml;
    # ".config/alacritty/ayu_dark.toml".source = common/terminal/ayu_dark.toml;
    # ".config/alacritty/solarized_light.toml".source = common/terminal/solarized_light.toml;
    # # Systemd
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
    SHELL = "fish";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.waybar = {
    enable = true;
    systemd.enable = true;
  };

  programs.ssh.matchBlocks = {
    nuc = {
      hostname = "txru.me";
      port = 65222;
      user = "lyterk";
      identityfile = "~/.ssh/id_ed25519";
    };
    git = {
      hostname = "txru.me";
      port = 65222;
      user = "git";
      identityfile = "~/.ssh/id_ed25519";
    };
    github = {
      hostname = "github.com";
      user = "git";
      identityfile = "~/.ssh/id_ed25519";
    };
  };

  # programs.doom-emacs = {
  #   enable = true;
  #   doomPrivateDir = ./doom;
  # };

  # programs.doomEmacs = pkgs.stdenv.mkDerivation {
  #   name = "doomInstall";
  #   src = doomSrc;
  #   buildInputs = [
  #     pkgs.emacs29
  #     pkgs.git
  #     (pkgs.ripgrep.override { withPCRE2 = true; })
  #   ];
  #   buildPhase = "${doomExecutable} install";
  #   installPhase = "cp -r . $out";
  # };

  services.emacs = {
    package = pkgs.emacs29;
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    maxCacheTtl = 86400;
    pinentryFlavor = "qt";
    # pinentry-rofi not an optional flavor. 
    # extraConfig = ''
    #  pinentry-program /run/current-system/sw/bin/pinentry-gtk2
    # '';
    # pinentryPackage available as of 24.0
    # pinentryPackage = pkgs.pinentry-rofi;
  };
}
