{ pkgs, lib, ... }:

let
  shellTools = with pkgs; [
    alacritty
    atuin
    direnv
    # graalvm-ce
    libtool
    pwgen
    gnupg
    gnumake
    keychain
  ];
  fonts = with pkgs; [
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    noto-fonts-monochrome-emoji
  ];
  programmingLanguages = with pkgs; [
    cargo
    # clojure
    # erlang
    # elixir
    # gleam
  ];
  gui = with pkgs; [
    gtklock
    imagemagick
  ];
  dataStores = with pkgs; [ sqlite ];
  collaboration = with pkgs; [
    # thunderbird
    # plantuml-c4
  ];
  fileViz = with pkgs; [
    calibre
    thunar
  ];
  fileSystems = with pkgs; [
    cdrtools
  ];
  studying = with pkgs; [ anki ];
  languageTools = with pkgs; [
    nil
    pyright
    black
    # elixir-ls
    rust-analyzer
    nodejs # Basically only for the copilot plugin
    beancount-language-server
  ];
  beam = with pkgs.beamMinimal28Packages; [
    # rebar3
  ];
  python = with pkgs.python312Packages; [
    ipython
    # debugpy # Handle this through uv?
    pyscaffold
    uv
  ];
  cpp = with pkgs; [
    # clang
    # clang-tools
    # llvmPackages_19.libcxx
  ];
in
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
    # ./nix/batteryNotifier.nix
    # ./nix/unstables/flake.nix
    ../../shared/firefox
    ../../shared/waybar
    ../../shared/sway
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages =
    with pkgs;
    [
      # phone interface
      # busybox
      # android-file-transfer
      duplicity # TODO https://github.com/NixOS/nixpkgs/issues/122671
      # notifications
      chromium
      # captive-browser
      mako # notifications
      blueman
      bluez
      bluez-tools
      # sound
      pulseaudio # used for getting and setting the volume
      pamixer
      # interfaces
      wl-clipboard
      pinentry-qt
      wev
      wob
      # pandoc
      inotify-tools
      xorg.xhost
      # parted
      # gparted
      # libreoffice-qt
      signal-desktop # out of date
      # video games
      # playonlinux
      innoextract
      # reading codes
      qrencode
      zbar
      # openrct2
      steam
      protonup-qt
      # torrents
      transmission_4-qt
      tor
      # monitors
      gimp
      grim # screenshots
      slurp # facilitate screenshots-- select a region in compositor.
      kanshi # managing monitors
      # pkgs-unstable.zed-editor
      # docker
      libgourou
      nix-du
      graphviz
      fastfetch
      qdirstat
    ]
    ++ shellTools
    ++ fonts
    ++ programmingLanguages
    ++ studying
    ++ collaboration
    ++ fileViz
    ++ fileSystems
    ++ dataStores
    ++ languageTools
    ++ python
    ++ cpp
    ++ gui
    ++ beam;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".gitignore".source = ../../shared/gitignore;
    ".config/gtklock/config.ini".text = ''
      [main]
      gtk-theme=Adwaita-dark
      style=.config/gtklock/layout.css
    '';
    ".config/gtklock/layout.css".text = ''
      window {
         background-image: url("/home/lyterk/Pictures/selectedBackgrounds/lockscreen.png");
         background-size: cover;
         background-repeat: no-repeat;
         background-position: center;
         background-color: gray;
         color: white;
      }
    '';
  };

  programs = {
    home-manager.enable = true;

    zsh = {
      enable = true;
      shellAliases = {
        ppush = "pass git push origin mainline";
        ppull = "pass git pull --rebase origin mainline";
        ls = "exa";
        vim = "nvim";
      };
    };

    fish.enable = true;

    keychain = {
      enable = true;
      enableFishIntegration = true;
      keys = [ "~/.ssh/id_ed25519" ];
    };

    rofi = {
      enable = true;
      font = "hack 13";
      theme = "solarized";
    };

    git = {
      enable = true;
      settings = {
        user.name = "Kevin Lyter";
        user.email = "code@lyterk.com";
        commit = {
          gpgsign = true;
        };
        init = {
          templateDir = "~/dotfiles/common/git_templates/";
          defaultBranch = "mainline";
        };
        pull = {
          rebase = true;
        };
        credential = {
          helper = "cache";
        };
        core = {
          excludesFile = "~/dotfiles/common/gitignore";
        };
      };
      signing.key = "0F39E83B";
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false;

      matchBlocks = {
        "*" = {
          user = "git";
          port = 22;
        };
        git = {
          hostname = "txru.me";
          port = 65222;
          user = "git";
          identityFile = "~/.ssh/id_ed25519";
        };
        github = {
          hostname = "github.com";
          user = "git";
          identityFile = "~/.ssh/id_ed25519";
        };
        desktop = {
          hostname = "txru.me";
          port = 65222;
          user = "lyterk";
          identityFile = "~/.ssh/id_ed25519";
        };
        # Not working atm
        plexProxy = {
          hostname = "txru.me";
          port = 65222;
          user = "lyterk";
          identityFile = "~/.ssh/id_ed25519";
          remoteForwards = [
            {
              bind.port = 8080;
              host.address = "127.0.0.1";
              host.port = 32400;
            }
          ];
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
      enable = true;
      package =
        with pkgs;
        ((emacsPackagesFor emacs30).emacsWithPackages (
          epkgs: with epkgs; [
            vterm
            treesit-grammars.with-all-grammars
          ]
        ));
    };
    # Redshift screen temperature
    gammastep = {
      enable = true;
      provider = "manual";
      latitude = 47.6;
      longitude = -122.3;
    };
    mako = {
      enable = true;
      settings = {
        default-timeout = 15000;
      };
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
      pinentry.package = pkgs.pinentry-qt;
    };
    kdeconnect = {
      enable = true;
    };

    swayidle = {
      enable = true;
      timeouts = [
        # Restart `swayidle` if adjusting timeouts
        {
          timeout = 300;
          command = "${pkgs.gtklock}/bin/gtklock -d";
        }
        {
          timeout = 300;
          command = ''swaymsg "output * dpms off"' resume 'swaymsg "output * dpms on"'';
        }
      ];
      events = [
        {
          event = "before-sleep";
          command = "${pkgs.gtklock}/bin/gtklock -d";
        }
      ];
    };
  };
  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "text/html" = [ "firefox.desktop" ];
      };
    };
  };
}
