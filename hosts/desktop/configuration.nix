# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware.nix
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
  # networking.hostName = "nixos"; # Define your hostname.
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

  # Configure console keymap
  console.keyMap = "dvorak";

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    users = {
      lyterk = {
        isNormalUser = true;
        description = "Kevin Lyter";
        extraGroups = [
          "networkmanager"
          "wheel"
        ];
        packages = with pkgs; [ ];
        shell = pkgs.fish;
      };
      git = {
        isSystemUser = true;
        group = "git";
        home = "/var/lib/git-server";
        createHome = true;
        shell = "${pkgs.git}/bin/git-shell";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBOdIT7nVDhMpH3CBlGKbyl6YR86IpRd5qRzN1gVnJ5s lyterk@nuc"
        ];
      };
    };
    groups.git = { };
  };

  home-manager.backupFileExtension = "backup";

  home-manager.users.lyterk =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        alacritty
        kdePackages.kdeconnect-kde
      ];
      programs = {
        fish = {
          enable = true;
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
          style = ../../shared/waybar/style.css;
        };
      };

      services = {
        gpg-agent = {
          enable = true;
          defaultCacheTtl = 3600;
          maxCacheTtl = 86400;
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
      home.stateVersion = "24.05";

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
    };

  programs = {
    light.enable = true;
    firefox.enable = true;

    fish = {
      enable = true;

      shellAliases = {
        vim = "nvim";
        ls = "exa";
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
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    #  wget
    #  thunderbird
    # shell utilities
    fish
    zsh
    fd
    eza
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
    alacritty
    atuin
    pwgen
    keychain
    # multimedia
    plex
    # accounting
    beancount-language-server
    # programming languages
    cargo
    clojure
    python3
    poetry
    elixir
    # secrets
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    rofi-pass-wayland
    gnupg
    pinentry-qt
    tailscale
    # nix stuff
    nil
    direnv
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
    syncthing
    # image hosting
    immich
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
  services = {
    gnome.gnome-keyring.enable = true;

    immich = {
      enable = true;
      port = 2283;
      mediaLocation = "/mnt/orange/immich";
    };

    nginx = {
      enable = true;
      virtualHosts = {
        "localhost" = {
          # enableACME = true;
          # forceSSL = true;
          locations."/" = {
            proxyPass = "http://[::1]:2283";
            proxyWebsockets = true;
            recommendedProxySettings = true;
            extraConfig = ''
              client_max_body_size 50000M;
              proxy_read_timeout   600s;
              proxy_send_timeout   600s;
              send_timeout         600s;
            '';
          };
        };
      };
    };

    plex = {
      enable = true;
      openFirewall = true;
    };

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      settings = {
        devices = {

        };
        gui = {
          user = "lyterk";
          password = "freddy";
        };
      };
    };

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

    # Enable CUPS to print documents.
    printing.enable = true;

    openssh = {
      enable = true;
      ports = [ 65222 ];
      settings = {
        PasswordAuthentication = false;
        AllowUsers = [
          "lyterk"
          "git"
        ];
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
