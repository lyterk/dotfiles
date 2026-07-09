# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  pkgs-unstable,
  ...
}:

let
  users = [
    {
      name = "lyterk";
      homedir = "/home/lyterk";
      targetUrl = "";
      secretFile = "/run/secrets/duplicityAws";
      gpgKeys = [
        "gpgCode"
        "gpgKev"
      ];
    }
    {
      name = "work";
      homedir = "/home/work";
      targetUrl = "";
      secretFile = "/run/secrets/duplicityAws";
      gpgKeys = [
        "gpgCode"
        "gpgKev"
      ];
    }
  ];

  duplicityUnits = map (
    u:
    config.duplicity.mkDuplicityUser {
      user = u.name;
      homedir = u.homedir;
      targetUrl = "s3://lyterk-backups-383137109783-us-west-2-an/hosts/miranda/users/${u.name}";
      secretFile = u.secretFile;
    }
  ) users;

in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware.nix
    ../../shared/sops.nix
    ../../shared/duplicity.nix
    # ../../shared/flutter.nix
    # ./home.nix
    # <home-manager/nixos>
  ];

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
  };

  boot = {
    # Bootloader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [ "mem_sleep_default=deep" ];
    # </ Bootloader>
    initrd.systemd.enable = true;
  };

  virtualisation.docker.enable = true;

  networking = {
    hostName = "miranda";

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

  environment.etc."libinput/local-overrides.quirks".text = ''
    [Serial Keyboards]
    MatchUdevType=keyboard
    MatchName=keyd virtual keyboard
    AttrKeyboardIntegration=internal
  '';

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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
  # console.keyMap = "dvorak";
  console.keyMap = "us";

  services = {
    # Enable CUPS to print documents.
    printing.enable = true;
    # vpn configuration
    tailscale.enable = true;
    # privacy vpn
    mullvad-vpn.enable = true;
    # For gpg key reading -- smart cards

    emacs.enable = true;

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

    # ollama = {
    #   enable = true;
    #   loadModels = [ "qwen3.6" ];
    #   acceleration = "cuda";
    # };

    atuin = {
      enable = true;
      # openRegistration = false;
      # host = "0.0.0.0";
      # port = 8888;
      # database.createLocally = true;
    };
    # anki-sync-server = {
    #   enable = true;
    #   address = "0.0.0.0";
    #   openFirewall = true;
    #   users = [
    #     {
    #       username = "lyterk";
    #       password = "freddy";
    #     }
    #   ];
    # };

    blueman.enable = true;

    # home-assistant = {
    #   enable = true;
    #   extraComponents = [
    #     # Components required to complete the onboarding
    #     "analytics"
    #     "google_translate"
    #     "met"
    #     "radio_browser"
    #     "shopping_list"
    #     # Recommended for fast zlib compression
    #     # https://www.home-assistant.io/integrations/isal
    #     "isal"
    #   ];
    #   config = {
    #     # Includes dependencies for a basic setup
    #     # https://www.home-assistant.io/integrations/default_config/
    #     default_config = { };
    #   };
    # };
    logind.settings.Login = {
      HandleLidSwitchDocked = "ignore";
      HandleLidSwitchExternalPower = "ignore";
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

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      settings = {
        gui = {
          user = "lyterk";
          password = "freddy";
        };
      };
    };

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

  };

  # Enable sound with pipewire.
  hardware.bluetooth.enable = true; # enabled by default
  hardware.graphics.enable = true;
  # hardware.pulseaudio.enable = true;

  security = {
    polkit.enable = true;
    rtkit.enable = true;
    # pam.services.sudo.u2fAuth = true;
  };

  systemd = {
    services = lib.mkMerge ((map (x: x.services) duplicityUnits));

    user.services = {
      kanshi = {
        description = "kanshi daemon";
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.kanshi}/bin/kanshi -c kanshi_config_file";
        };
      };

      emacs.serviceConfig = {
        Slice = "session.slice";
      };
    };

  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lyterk = {
    shell = pkgs.fish;
    isNormalUser = true;
    description = "Kevin Lyter";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "docker"
    ];
  };

  users.users.work = {
    shell = pkgs.fish;
    isNormalUser = true;
    description = "Yotta work profile";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "docker"
    ];
  };

  home-manager.users.work = ./users/work-home.nix;
  home-manager.users.lyterk = ./users/lyterk-home.nix;

  # home-manager.users.lyterk = import /home/lyterk/.config/home-manager/home.nix;

  programs = {
    # sway.enable = true;
    # System-wide I guess?
    # Mounting phones with mtp
    fuse = {
      mountMax = 1000;
      userAllowOther = true;
    };
    kdeconnect.enable = true;

    fish = {
      enable = true;

      shellAliases = {
        ppush = "pass git push origin mainline";
        ppull = "pass git pull --rebase origin mainline";
        ls = "exa";
        vim = "nvim";
      };
      # Making this per-user
      # loginShellInit = ''
      #   if test (id --user $USER) -ge 1000 && string match -qr '/dev/tty[0-9]' (tty)
      #     exec sway
      #   end
      # '';
    };
    # steam.enable = true;
  };

  # Building raspberry pis
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  nix = {
    # Also for building pis
    settings.extra-platforms = config.boot.binfmt.emulatedSystems;
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    settings.experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  # https://github.com/NixOS/nixpkgs/issues/240886
  security.pam.services.gtklock = { };

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
      # nix specific
      home-manager
      nixfmt-rfc-style
      nix-ld
      # build
      gcc
      # editors
      neovim
      # shells
      fish
      zsh
      # shell utilities
      bat
      eza
      fd
      git
      jq
      xan
      rlwrap
      htop
      unzip
      ripgrep
      tree
      wget
      # network
      tailscale
      mullvad-vpn
      # phone connection
      kdePackages.kdeconnect-kde
      kdePackages.okular
      # wm
      sway
      sops # secrets
      xwayland # necessary for proxying x connections for wayland
      age
      bluez # necessary for home-assistant
      # android-studio
      # androidenv.androidPkgs.androidsdk
      # window manager
      greetd
      # AI yo
      pkgs-unstable.code-cursor
      nodejs_22
      pnpm
      awscli2
      claude-code
      # tidal
      # tidal-dl
    ];
  };

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

  # Connecting devices via USB to calibre
  # https://nixos.wiki/wiki/Calibre
  services.udisks2.enable = true;

  # Open ports in the firewall.

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
