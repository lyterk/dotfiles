{ pkgs, lib, ... }:

let
  shellTools = with pkgs; [
    alacritty
    atuin
    cmake
    direnv
    # graalvm-ce
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
  ];
  dataStores = with pkgs; [ sqlite ];
  collaboration = with pkgs; [
    thunderbird
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
    # pymavlink
    pyscaffold
    uv
    # tree-sitter-grammars.tree-sitter-python
  ];
  cpp = with pkgs; [
    clang
    clang-tools
    # llvmPackages_19.libcxx
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
    # ./nix/unstables/flake.nix
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages =
    with pkgs;
    [
      deja-dup # TODO https://github.com/NixOS/nixpkgs/issues/122671
      # notifications
      chromium
      captive-browser
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
      pandoc
      xorg.xhost
      parted
      gparted
      libreoffice-qt
      signal-desktop # out of date
      # video games
      # playonlinux
      innoextract
      # openrct2
      steam
      protonup-qt
      # torrents
      transmission-qt
      tor
      # monitors
      grim # screenshots
      slurp # facilitate screenshots-- select a region in compositor.
      kanshi # managing monitors
      # pkgs-unstable.zed-editor
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
    ++ cpp
    ++ javascript
    ++ accounting
    ++ gui;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # ".gitconfig".source = common/gitconfig;
    ".gitignore".source = common/gitignore;
    ".pip/pip.conf".source = common/pip.conf;
    ".config/flake8".source = common/flake8;
    ".config/rustfmt/rustfmt.toml".source = common/rustfmt.toml;
    ".zshenv".source = common/zshenv;
    ".zshrc".source = common/zshrc;
    ".sbclrc".source = common/sbclrc;
    # ".config/fish/config.fish".source = common/fish/config.fish;
    ".config/fish/functions/ssh_agent.fish".source = common/fish/functions/ssh_agent.fish;
    ".config/mimeapps.list".source = common/mimeapps.list;
    ".config/rofi/config.rasi".source = common/rofi_config;
    # ".config/sway/config".source = common/sway/sway_config;
    # TODO Figure out the emoji https://github.com/Alexays/Waybar/wiki/Examples
    # ".config/kanshi/config".source = common/sway/kanshi_config;
    # ".config/waybar/config".source = common/sway/waybar_config;
    # ".config/waybar/style.css".source = common/sway/waybar_style.css;
    ".config/alacritty/alacritty_base.toml".text = ''
      shell = "/usr/bin/env fish"

      [window]
      opacity = 0.9
    '';
    ".config/gtklock/config.ini".text = ''
      [main]
      gtk-theme=Adwaita-dark
      style=.config/gtklock/layout.css
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
  # home.sessionVariables = {
  #   EDITOR = "emacsclient -t";
  #   # SHELL = "fish";
  #   BROWSER = "${pkgs.firefox}/bin/firefox";
  #   # History in elixir and erlang shells
  #   ERL_AFLAGS = "-kernel shell_history enabled";
  #   # Sound bar
  #   WOBSOCK = "$XDG_RUNTIME_DIR/wob.sock";
  # };

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

    git = {
      enable = true;
      userEmail = "code@lyterk.com";
      userName = "Kevin Lyter";
      signing.key = "0F39E83B";

      extraConfig = {
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
    };

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
      settings = {
        mainBar = {
          position = "top";
          height = 24;
          modules-left = [
            "sway/workspaces"
            "sway/mode"
            "sway/scratchpad"
            "custom/media"
          ];
          modules-center = [ "sway/window" ];
          modules-right = [
            "idle_inhibitor"
            "temperature"
            "cpu"
            "memory"
            "network"
            "pulseaudio"
            "backlight"
            "keyboard-state"
            "battery"
            "battery#bat2"
            "tray"
            "clock"
          ];
          "sway/mode" = {
            "format" = "<span style=\"italic\">{}</span>";
          };
          "sway/scratchpad" = {
            "format" = "{icon} {count}";
            "show-empty" = false;
            "format-icons" = [
              ""
              ""
            ];
            "tooltip" = true;
            "tooltip-format" = "{app}: {title}";
          };
          "idle_inhibitor" = {
            "format" = "{icon}";
            format-icons = [
              ""
              ""
            ];
          };
          keyboard-state = {
            numlock = "true";
            capslock = "true";
            format = "{name} {icon}";
            format-icons = [
              ""
              ""
            ];
          };
          tray = {
            spacing = 10;
          };

          clock = {
            "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
            "format" = "{:L%Y-%m-%d<small>[%a]</small> <tt><small>%p</small></tt>%I:%M}";
          };
          cpu = {
            "format" = " {usage}%";
          };
          memory = {
            "format" = " {}%";
          };
          temperature = {
            thermal-zone = 2;
            hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";
            critical-threshold = 80;
            format-critical = "{icon} {temperatureC}°C";
            format = "{icon} {temperatureC}°C";
            format-icons = [
              ""
              ""
              ""
            ];
          };
          backlight = {
            format = "{icon} {percent}%";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
            ];
          };
          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{icon} {capacity}%";
            format-charging = " {capacity}%";
            format-plugged = " {capacity}%";
            format-alt = "{icon} {time}";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };
          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = " {ifname}";
            tooltip-format = " {ifname} via {gwaddr}";
            format-linked = " {ifname} (No IP)";
            format-disconnected = "Disconnected ⚠ {ifname}";
            format-alt = " {ifname}: {ipaddr}/{cidr}";
          };
          pulseaudio = {
            scroll-step = 5; # %, can be a float
            format = "{icon} {volume}% {format_source}";
            format-bluetooth = " {icon} {volume}% {format_source}";
            format-bluetooth-muted = "  {icon} {format_source}";
            format-muted = "  {format_source}";
            format-source = " {volume}%";
            format-source-muted = "";
            format-icons = {
              default = [
                ""
                ""
                ""
              ];
            };
            on-click = "pavucontrol";
            on-click-right = "foot -a pw-top pw-top";
          };
        };
      };
      style = ./common/sway/waybar_style.css;
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
      checkConfig = false;
      extraConfig = "exec rm -f $WOBSOCK && mkfifo $WOBSOCK && tail -f $WOBSOCK | wob";
      config = rec {
        terminal = "alacritty";
        modifier = "Mod4";
        # Provided by swaybar
        bars = [ ];
        assigns = {
          "${ws1}" = [ { app_id = "firefox"; } ];
          "${ws2}" = [ { app_id = "Alacritty"; } ];
          "${ws3}" = [ { class = "Emacs"; } ];
          "${ws4}" = [ { class = "Signal"; } ];
        };
        output = {
          "*" = {
            bg = "/home/lyterk/Pictures/backgrounds/presque-ile.png fill";
          };
        };
        input = {
          "*" = {
            xkb_layout = "us,es";
            xkb_variant = "dvorak,dvorak";
            xkb_options = "ctrl:nocaps,grp:rctrl_toggle";
          };
        };

        keybindings = lib.mkOptionDefault {
          "${modifier}+f2" = "exec ${pkgs.firefox}/bin/firefox";
          "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
          "${modifier}+p" = "exec ~/dotfiles/scripts/passmenu";
          "Shift+Print" = "exec ${pkgs.grim}/bin/grim ~/Pictures/screenshots/$(date +'%Y-%m-%d_%H-%M-%S_screenshot.png')";
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
          "${modifier}+r" = "mode resize";
        };

        modes = {
          kevin = {
            "c" = "exec ${pkgs.calibre}/bin/calibre; mode default";
            "g" = "exec ${pkgs.chromium}/bin/chromium; mode default";
            "e" = "exec ${pkgs.emacs}/bin/emacsclient -c; mode default";
            "s" = "exec ${pkgs.signal-desktop}/bin/signal-desktop; mode default";
            "w" = "exec ~/dotfiles/scripts/rofi-wifi-menu.sh; mode default";
            "j" = "exec ${pkgs.rofimoji}/bin/rofimoji; mode default";
            "v" = "exec ${pkgs.vlc}/bin/vlc; mode default";
            "Escape" = "mode default";
            "Return" = "mode default";
          };
          resize = {
            Down = "resize grow height 10 px";
            Escape = "mode default";
            Left = "resize shrink width 10 px";
            Return = "mode default";
            Right = "resize grow width 10 px";
            Up = "resize shrink height 10 px";
            h = "resize shrink width 10 px";
            j = "resize grow height 10 px";
            k = "resize shrink height 10 px";
            l = "resize grow width 10 px";
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
        ((emacsPackagesFor emacs29).emacsWithPackages (
          epkgs: with epkgs; [
            vterm
            treesit-grammars.with-all-grammars
          ]
        ));
    };
    mako = {
      enable = true;
      defaultTimeout = 15000;
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
}
