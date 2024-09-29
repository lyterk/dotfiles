{
  pkgs,
  config,
  lib,
  ...
}:

let
  # doomSrc = pkgs.fetchFromGitHub {
  #   owner = "doomemacs";
  #   repo = "doomemacs";
  #   rev = "9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee";
  # };
  # doomConfigSrc =
  #   config.lib.file.mkOutOfStoreSymlink "/home/lyterk/.config/doom";
  # doomExecutable = "${doomSrc}/bin/doom";

  shellTools = with pkgs; [
    alacritty
    atuin
    cmake
    direnv
    graalvm-ce
    libtool
    pwgen
    gnupg
    gnumake
    keychain
    node2nix
  ];
  fonts = with pkgs; [
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    noto-fonts-monochrome-emoji
  ];
  accounting = with pkgs; [
    beancount
    fava
    beancount-language-server
  ];
  programmingLanguages = with pkgs; [
    cargo
    clojure
    nodejs_22
    python3
    poetry
    elixir
  ];
  gui = with pkgs; [
    gtklock
    imagemagick
    rofi
    sway
  ];
  dataStores = with pkgs; [ sqlite ];
  collaboration = with pkgs; [
    thunderbird
    beeper
    plantuml-c4
  ];
  fileViz = with pkgs; [
    calibre
    xfce.thunar
  ];
  studying = with pkgs; [ anki ];
  languageTools = with pkgs; [
    nil
    cljfmt
    clojure-lsp
    pyright
    black
    elixir-ls
    rust-analyzer
  ];
  python = with pkgs.python311Packages; [
    bpython
    ipython
    pytest
    # tree-sitter-grammars.tree-sitter-python
  ];
  javascript = with pkgs; [
    yarn
    # nodePackages.prettier
    # tree-sitter-grammars.tree-sitter-typescript
  ];
in
# elixir = with pkgs;
#   with tree-sitter-grammars; [
#     tree-sitter-elixir
#     tree-sitter-heex
#   ];
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "lyterk";
  home.homeDirectory = "/home/lyterk";

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };

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
  home.packages =
    with pkgs;
    [
      deja-dup # TODO https://github.com/NixOS/nixpkgs/issues/122671
      # notifications
      chromium
      mako
      # sound
      pulseaudio # used for getting and setting the volume
      pamixer
      # interfaces
      wl-clipboard
      pinentry-qt
      wev
      wob
      pandoc
      libreoffice-qt
      signal-desktop # out of date
      # video games
      playonlinux
      innoextract
      openrct2
      steam
      # torrents
      transmission-qt
      # monitors
      grim # screenshots
      slurp # facilitate screenshots-- select a region in compositor.
      kanshi # managing monitors
      # network
    ]
    ++ shellTools
    ++ fonts
    ++ programmingLanguages
    ++ studying
    ++ collaboration
    ++ fileViz
    ++ dataStores
    ++ languageTools
    ++ python
    ++ javascript
    ++ accounting
    ++ gui;

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
    # ".config/sway/config".source = common/sway/sway_config;
    # TODO Figure out the emoji https://github.com/Alexays/Waybar/wiki/Examples
    # ".config/kanshi/config".source = common/sway/kanshi_config;
    ".config/waybar/config".source = common/sway/waybar_config;
    ".config/waybar/style.css".source = common/sway/waybar_style.css;
    ".config/alacritty/alacritty_base.toml".text = ''
      shell = "/usr/bin/env fish"

      [window]
      opacity = 0.9
    '';
    ".config/gtklock/config.ini".text = ''
      [main]
      gtk-theme=Adwaita-dark
      style=layout.css
    '';
    ".config/gtklock/layout.css".text = ''
      window {
         background-image: url("/home/lyterk/Pictures/backgrounds/lehighton.png");
         background-size: cover;
         background-repeat: no-repeat;
         background-position: center;
         background-color: gray;
         color: white;
      }
    '';
    # ".ssh/config".source = "common/ssh_config";
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
    # History in elixir and erlang shells
    ERL_AFLAGS = "-kernel shell_history enabled";
  };

  programs = {
    home-manager.enable = true;

    firefox = {
      enable = true;
      profiles = {
        default = {
          id = 0;
          name = "defaultNix";
          isDefault = true;
          settings = {
            "browser.aboutConfig.showWarning" = false;
            "browser.tabs.closeWindowWithLastTab" = false;
            "browser.startup.homepage" = "https://wikipedia.org";
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "extensions.pocket.enabled" = false;
          };
          userChrome = ''
            @namespace url(http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul);

            /* hides the native tabs */

            #TabsToolbar {
              visibility: collapse !important;
            }
          '';
        };
      };
    };

    waybar = {
      enable = true;
      systemd.enable = true;
    };

    ssh.matchBlocks = {
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
  };

  wayland.windowManager.sway =
    let
      ws1 = "1:browser";
      ws2 = "2:terminal";
      ws3 = "3:emacs";
      ws4 = "4:signal";
    in
    {
      enable = true;
      systemd.enable = true;
      checkConfig = true;
      config = rec {
        terminal = "alacritty";
        modifier = "Mod4";
        # Provided by swaybar
        bars = [ ];
        input = {
          "*" = {
            xkb_layout = "us";
            xkb_variant = "dvorak";
            xkb_options = "ctrl:nocaps";
          };
        };

        keybindings = lib.mkOptionDefault {
          "${modifier}+f2" = "exec ${pkgs.firefox}";
          "${modifier}+d" = "exec ${pkgs.rofi} -show drun";
          "${modifier}+p" = "exec ~/dotfiles/scripts/passmenu";
          "Shift+Print" = "exec grim ~/Pictures/screenshots/$(date +'%Y-%m-%d_%H-%M-%S_screenshot.png')";
          # Switch to workspace
          "${modifier}+1" = "workspace number ${ws1}";
          "${modifier}+2" = "workspace number ${ws2}";
          "${modifier}+3" = "workspace number ${ws3}";
          "${modifier}+4" = "workspace number ${ws4}";
          # Move container to workspace
          "${modifier}+Shift+1" = "move container to workspace number $ws1; workspace number ${ws1}";
          "${modifier}+Shift+2" = "move container to workspace number $ws2; workspace number ${ws2}";
          "${modifier}+Shift+3" = "move container to workspace number $ws3; workspace number ${ws3}";
          "${modifier}+Shift+4" = "move container to workspace number $ws4; workspace number ${ws4}";
          # Brightness
          "XF86MonBrightnessDown" = "exec light -U 10";
          "XF86MonBrightnessUp" = "exec light -A 10";
          # Loudness
          "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle && pamixer --get-volume > $WOBSOCK";
          "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +5% && pamixer --get-volume > $WOBSOCK";
          "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -5% && pamixer --get-volume > $WOBSOCK";
          # Personal mode
          "${modifier}+m" = "mode kevin";
        };

        modes = {
          kevin = {
            "c" = "exec ${pkgs.calibre}";
            "g" = "exec ${pkgs.chromium}";
            "e" = "exec emacsclient -c";
            "s" = "exec ${pkgs.signal-desktop}";
            "w" = "exec ~/dotfiles/scripts/rofi-wifi-menu.sh";
            "j" = "exec ${pkgs.rofimoji}";
            "v" = "exec ${pkgs.vlc}";
          };
        };
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

  # services.mullvad-vpn.enable = true;

  services = {
    emacs = {
      package =
        with pkgs;
        ((emacsPackagesFor emacs29).emacsWithPackages (
          epkgs: with epkgs; [
            vterm
            treesit-grammars.with-all-grammars
          ]
        ));
      enable = true;
    };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 3600;
      maxCacheTtl = 86400;
      # pinentryFlavor = "qt";
      # pinentry-rofi not an optional flavor.
      # extraConfig = ''
      #  pinentry-program /run/current-system/sw/bin/pinentry-gtk2
      # '';
      # pinentryPackage available as of 24.0
      pinentryPackage = pkgs.pinentry-qt;
    };
  };
}
