{ pkgs, lib, ... }:

let
  shellTools = with pkgs; [
    atuin
    direnv
    # graalvm-ce
    libtool
    pwgen
    gnupg
    gnumake
    keychain
    mermaid-cli
    ripgrep-all # searching in epubs etc.
    pandoc
    wl-clipboard
    xan
    zip
    yt-dlp
  ];
  interfaces = with pkgs; [
    blueman
    bluez
    bluez-tools
    grim # screenshots
    kanshi # managing monitors
    slurp # facilitate screenshots-- select a region in compositor.
    inotify-tools
    pinentry-qt
    pulseaudio # used for getting and setting the volume
    pamixer
    wev
    wob
    qrencode
    libgourou # Adobe DRM removal
  ];
  environments = with pkgs; [
    docker
    docker-compose
  ];
  fonts = with pkgs; [
    font-awesome
    noto-fonts
    # noto-fonts-cjk-sans
    noto-fonts-color-emoji
    # noto-fonts-monochrome-emoji
    nerd-fonts.jetbrains-mono
  ];
  programmingLanguages = with pkgs; [
    # cargo
    # clojure
    # erlang
    # elixir
    # go
    # gleam
    # fvm
    rustup
  ];
  gui = with pkgs; [
    gtklock # lock screen
    imagemagick # Software suite to create, edit, compose, or convert bitmap images
    poppler-utils # pdf-rendering
    # tesseract # OCR
    mako # notifications
    libnotify
    wlr-which-key
    yazi
  ];
  dataStores = with pkgs; [ sqlite ];
  collaboration = with pkgs; [
    libreoffice
    gimp
    thunderbird
    obs-studio
  ];
  fileSystems = with pkgs; [
    # cdrtools # cd reading
    calibre
    thunar
    duplicity # TODO https://github.com/NixOS/nixpkgs/issues/122671
  ];
  studying = with pkgs; [ anki ];
  languageTools = with pkgs; [
    nil
    pyright
    black
    # elixir-ls
    # rust-analyzer
    nodejs # Basically only for the copilot plugin
    # beancount-language-server
  ];
  python = with pkgs.python313Packages; [
    ipython
    # debugpy # Handle this through uv?
    pyscaffold # project generator
    uv
  ];
  # homeAssistant = [];
  # selfHostedModels = with pkgs; [
  #   ollama-cuda
  # ];
in
{
  home.stateVersion = "23.11";

  home.packages =
    shellTools
    ++ interfaces
    ++ environments
    ++ fonts
    ++ programmingLanguages
    ++ gui
    ++ dataStores
    ++ collaboration
    ++ fileSystems
    ++ studying
    ++ languageTools
    ++ python;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".gitignore".source = ./gitignore;
    ".config/xdg-desktop-portal-termfilechooser/config".text = ''
      [filechooser]
      cmd=/run/current-system/sw/bin/yazi-filepicker.sh
    '';
  };

  home.sessionVariables = {
    EDITOR = "nvim";
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

    fish = {
      enable = true;
      interactiveShellInit = ''
        atuin init fish | source
      '';
      functions = {
        yy = {
          body = ''
            set tmp (mktemp -t "yazi-cwd.XXXXXX")
            yazi $argv --cwd-file="$tmp"
            set cwd (cat -- $tmp)
            if test -n "$cwd" && test "$cwd" != "$PWD"
                cd -- $cwd
            end
            rm -f -- $tmp
          '';
        };
      };
    };

    foot = {
      enable = true;
      # enableFishIntegration = true;
      settings = {
        main = {
          font = lib.mkForce "FreeMono:size=12";
        };
        scrollback = {
          lines = 100000;
        };
      };
    };

    keychain = {
      enable = true;
      enableFishIntegration = true;
      keys = [ "~/.ssh/id_ed25519" ];
    };

    rofi = {
      enable = true;
      # font = "hack 13";
      # theme = "solarized";
    };

    git = {
      enable = true;
      settings = {
        pull.rebase = true;
        credential.helper = "cache";
        init.defaultBranch = "mainline";
      };
    };
  };

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
    wlsunset = {
      enable = true;
      latitude = 47.6;
      longitude = -122.3;
    };
    # wayland notifications
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
    gnome-keyring = {
      enable = true;
      components = [
        "secrets"
        "ssh"
      ]; # optional; omit for defaults
    };

    kanshi = {
      enable = true;
      settings = [
        {
          profile.name = "undocked";
          profile.outputs = [
            {
              criteria = "eDP-1";
              scale = 1.0;
              status = "enable";
            }
          ];
        }
        {
          profile.name = "docked";
          profile.outputs = [
            {
              criteria = "eDP-1";
              scale = 1.0;
              status = "disable";
              position = "3440,0";
            }
            {
              criteria = "Dell Inc. DELL S3422DWG BNT3KK3";
              scale = 1.0;
              status = "enable";
              position = "0,0";
            }
          ];
        }
      ];
    };

    # kdeconnect = {
    #   enable = true;
    # };

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
      events = {
        before-sleep = "${pkgs.gtklock}/bin/gtklock -d";
      };
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
    portal = {
      extraPortals = [
        pkgs.xdg-desktop-portal-wlr
        pkgs.xdg-desktop-portal-termfilechooser
      ];
      config.common = {
        default = "wlr";
        "org.freedesktop.impl.portal.FileChooser" = "termfilechooser";
      };
    };
  };
}
