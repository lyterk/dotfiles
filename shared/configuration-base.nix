{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  yaziFilePicker = pkgs.writeShellScriptBin "yazi-filepicker.sh" ''
    echo "Called with: $@" >> /tmp/yazi-picker.log
    ${pkgs.foot}/bin/foot -e ${pkgs.yazi}/bin/yazi  --chooser-file="$5"
  '';
  refreshConfigScript = pkgs.writeShellScriptBin "refreshNix.sh" ''
      SUDO_PASSWORD=$(rofi -dmenu -password -no-fixed-num-lines -p "[sudo] password for $USER: ")
      REBUILD_COMMAND="echo \"$SUDO_PASSWORD\" | sudo -S ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch --flake /etc/nixos#miranda"
      if eval "$REBUILD_COMMAND"; then
      ${pkgs.libnotify}/bin/notify-send \
        --urgency=normal \
        --icon=emblem-default \
        "NixOS Rebuild" \
        "Configuration switched successfully ✓"
    else
      ${pkgs.libnotify}/bin/notify-send \
        --urgency=critical \
        --icon=dialog-error \
        "NixOS Rebuild" \
        "Rebuild failed ✗"
    fi
  '';

  mkU2fMapping = username: ''
    # oberon
    ${username}:/O9GzHla8dBfV1xiSYRDNjcmd8jt+JWNhpvJYQeZMvn6aTuml82v2/WP1TV4NtbY9AXCvr7b9mNZbr4CnJMYjWOIiZOp58GvrEXwRSUMxC3Y3S9ApLhogs+c3LniEyhg,O0J0U8hKVWHsXw3RccQnP4Vp/GoAK8qBgF3Tm5UbE9OiVSXe5XyWYB+c8p1BmjSNvng5aJBU5e/0t6ZdySbmkg==,es256,+presence

    # horatio
    ${username}:ixQHaz3f6ry/Q7CXXeA/1F9CZSRGD5VVGXFEv59FY15nkYl19Fi0H8rpKFfGQ+IgB9JIcx8M5Exzx9oWuVrEk4N1xk3nbmHYE3CrLg3FAh4Dnm0zlYe64LKLJeUdaSz4,U7KqjFrOVo9kntDClTyvBQ7swV9dcNL0GGlVJ6ncs83UEx7QUSaLDGZHR+A+uUjQSgXKen1AM+3UdcCrdkWl/Q==,es256,+presence

    # desdemona
    ${username}:MUCS5cu6PVjs7oER1iUWo7bEuyhzhwgCMTUXMY3LP/Tl+lxjJJC9Lmq5FI8sZeXep6LfRtdm7t9kCzLQMAjaZGuJ3pH/5/ZY+/uQPjtE+Ni8HiyuXBn8ks+ve0wFYAV8,AN8w1w35NYghnhV4COcn3tvN+aPwptoYItsbkIdCZud7/XQ8GxywbOHowV9goqsoO6xm3Em5+vAsmAWepAxChw==,es256,+presence

    # yotta
    ${username}:k4r1o2GJnKWpx/SWAOZ66psAlB8oNwoog5yMUgwPpvJPBUcGwdhO2Dc13AYoWabXOOMsqpxTlPILdn/BBOqBPg==,5hGxVmOVPnUBOPWWb1ni3i3xEfxubk8Fg3bamopkxe++en8Du1afYz65B8/afJtedVT638SLz1SVpkOKpUGoJA==,es256,+presence
  '';
  targetUsers = [
    "lyterk"
    "work"
  ];
  combinedU2fMappings = builtins.concatStringsSep "\n" (map mkU2fMapping targetUsers);
in
{
  imports = [
    ./sops.nix
    ./restic.nix
  ];

  time.timeZone = "America/Los_Angeles";

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
  networking = {

    nameservers = [
      "1.1.1.1"
      "1.0.0.1"
    ];
    networkmanager.enable = true;

    firewall = rec {
      enable = true;
      allowedTCPPortRanges = [
        {
          # kdeconnect range
          from = 1714;
          to = 1764;
        }
      ];
      allowedUDPPortRanges = allowedTCPPortRanges;
    };
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;
    iproute2.enable = true; # ostensibly useful for mullvad
  };
  nix = {
    # Also for building pis
    settings = {
      extra-platforms = config.boot.binfmt.emulatedSystems;
      experimental-features = [
        "nix-command"
        "flakes"
        "pipe-operators"
      ];
    };
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    extraOptions = ''
      !include ${config.sops.secrets.githubToken.path}
    '';
  };

  environment.etc."libinput/local-overrides.quirks".text = ''
    [Serial Keyboards]
    MatchUdevType=keyboard
    MatchName=keyd virtual keyboard
    AttrKeyboardIntegration=internal
  '';

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-wlr ];
    config.common.default = "wlr";
  };

  security = {
    polkit.enable = true;
    rtkit.enable = true;
    # https://github.com/NixOS/nixpkgs/issues/240886
    pam.services.gtklock = { };
    # text = ''
    #   auth      sufficient  pam_u2f.so
    #   auth      include     login
    #   account   include     login
    #   session   include     login
    # '';
    pam = {
      services.sudo.u2fAuth = true;
      u2f = {
        enable = false; # do not enable, will require FIDO for login
        control = "required";
        settings = {
          cue = true;
          authFile = pkgs.writeText "u2f-mappings" combinedU2fMappings;
        };
      };
    };
  };

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
  };

  services = {
    blueman.enable = true;
    # Enable CUPS to print documents.
    printing.enable = true;
    # vpn configuration
    tailscale.enable = true;
    # privacy vpn
    mullvad-vpn.enable = true;
    emacs.enable = true;
    pcscd.enable = true;
    pulseaudio.enable = false;
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
    atuin = {
      enable = true;
      # openRegistration = false;
      # host = "0.0.0.0";
      # port = 8888;
      # database.createLocally = true;
    };
    privoxy = {
      enable = true;
      settings = {
        listen-address = "127.0.0.01:8118";
      };
    };

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --remember --cmd sway";
          user = "lyterk";
        };
      };
    };
    logind.settings.Login = {
      HandleLidSwitchDocked = "ignore";
      # HandleLidSwitchExternalPower = "ignore";
    };

    keyd = {
      enable = true;
      keyboards = {
        default = {
          ## the id of your keyboard taken from the monitor command - specifying it here and not using a wildcard * might avoid the aforementioned libinput issue with palm rejection.
          ids = [ "0001:0001:70533846" ];
          settings = {
            main = {
              ## taking the key combination from the monitor command and remapping it to meta / super key
              "f23" = "rightcontrol";
            };
          };
        };
      };
    };

    # syncthing = {
    #   enable = true;
    #   openDefaultPorts = true;
    #   settings = {
    #     gui = {
    #       user = "lyterk";
    #       password = "freddy";
    #     };
    #   };
    # };
    resolved = {
      enable = true;
      settings.Resolve = {
        DNSSEC = true;
        DNSOverTLS = true;
        Domains = [ "~." ];
        FallbackDNS = [
          "1.1.1.1#one.one.one.one"
          "1.0.0.1#one.one.one.one"
        ];
      };
    };
    udev.packages = [ pkgs.yubikey-personalization ];

    # Connecting devices via USB to calibre
    # https://nixos.wiki/wiki/Calibre
    udisks2.enable = true;
  };

  hardware = {
    # Enable sound with pipewire.
    bluetooth.enable = true; # enabled by default
    graphics.enable = true;
    # hardware.pulseaudio.enable = true;
  };

  programs = {
    fish = {
      enable = true;
      shellAliases = {
        ppush = "pass git push origin mainline";
        ppull = "pass git pull --rebase origin mainline";
        ls = "exa";
        vim = "nvim";
      };
    };
    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        stdenv.cc.cc.lib
      ];
    };
  };

  home-manager.users.lyterk = ./users/lyterk-home.nix;
  home-manager.extraSpecialArgs = { inherit pkgs-unstable; };

  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    variables = {
      EDITOR = "emacsclient -t";
      # SHELL = "fish";
      BROWSER = "${pkgs.firefox}/bin/firefox";
      # History in elixir and erlang shells
      ERL_AFLAGS = "-kernel shell_history enabled";
      # Sound bar
      WOBSOCK = "$XDG_RUNTIME_DIR/wob.sock";
      ANDROID_HOME = "${pkgs.androidenv.androidPkgs.androidsdk}/libexec/android-sdk";
    };

    systemPackages = with pkgs; [
      age
      awscli2
      bat
      bluez # necessary for home-assistant
      eza
      fd
      ffmpeg
      fish
      foot
      gcc-unwrapped
      git
      greetd
      home-manager
      kdePackages.okular
      htop
      jq
      mullvad-vpn
      neovim
      cowsay
      nix-ld
      nixfmt
      pam_u2f
      pkg-config
      refreshConfigScript
      restic
      ripgrep
      rlwrap
      sops # secrets
      sway
      tailscale
      tree
      unzip
      wget
      wlr-randr
      xan # processing CSVs
      xdg-desktop-portal-termfilechooser
      xwayland # necessary for proxying x connections for wayland
      yazi # file browser
      yaziFilePicker
      zsh
    ];
  };
}
