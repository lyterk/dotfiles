# Common configuration enabled on all hosts.
#
{
  config,
  lib,
  pkgs,
  flakeInputs,
  ...
}:
{
  config = {
    #############################################################################
    ## General
    #############################################################################

    # The NixOS release to be compatible with for stateful data such as databases.
    # system.stateVersion = "24.11";

    # Only keep the last 500MiB of systemd journal.
    # services.journald.extraConfig = "SystemMaxUse=500M";

    # Collect nix store garbage and optimise daily.
    nix.gc.automatic = true;
    nix.gc.options = "--delete-older-than 30d";
    nix.optimise.automatic = true;

    # Enable flakes
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    # Clear out /tmp after a fortnight and give all normal users a ~/tmp
    # cleaned out weekly.
    # systemd.tmpfiles.rules =
    #   [ "d /tmp 1777 root root 14d" ]
    #   ++ (
    #     let
    #       mkTmpDir = n: u: "d ${u.home}/tmp 0700 ${n} ${u.group} 7d";
    #     in
    #     mapAttrsToList mkTmpDir (filterAttrs (_: u: u.isNormalUser) config.users.users)
    #   );

    # Enable passwd and co.
    # users.mutableUsers = true;

    # Upgrade packages and reboot if needed
    # system.autoUpgrade.enable = true;
    # system.autoUpgrade.allowReboot = true;
    # system.autoUpgrade.flags = [ "--recreate-lock-file" ];
    # system.autoUpgrade.flake = "/etc/nixos";
    # system.autoUpgrade.dates = "06:45";

    # Reboot on panic and oops
    # https://utcc.utoronto.ca/~cks/space/blog/linux/RebootOnPanicSettings
    # boot.kernel.sysctl = {
    #   "kernel.panic" = 10;
    #   "kernel.panic_on_oops" = 1;
    # };

    #############################################################################
    ## Locale
    #############################################################################

    # Set your time zone.
    time.timeZone = "America/Los_Angeles";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";
    # Keyboard
    console.keyMap = "dvorak";

    #############################################################################
    ## Firewall
    #############################################################################

    #############################################################################
    ## Services
    #############################################################################

    services = {
      gnome.gnome-keyring.enable = true;

      # syncthing = {
      #   enable = true;
      #   openDefaultPorts = true;
      #   settings = {
      #     devices = {

      #     };
      #     gui = {
      #       user = "lyterk";
      #       password = "freddy";
      #     };
      #   };
      # };

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

    ## User accounts
    #############################################################################

    #############################################################################
    ## Package management
    #############################################################################

    # gnupg doesn't come with pinentry, so require the agent
    programs.gnupg.agent.enable = true;

    # Allow packages with non-free licenses.
    nixpkgs.config.allowUnfree = true;

    # System-wide packages
    environment.systemPackages = with pkgs; [
      aspell
      aspellDicts.en
      bind
      fd
      file
      fzf
      git
      gnupg
      htop
      lsof
      python3
      ripgrep
      rsync
      shellcheck
      # smartmontools
      tmux
      unzip
      vim
      wget
      which
      whois

      nixfmt-rfc-style
      nix-ld
      # build
      gcc
      # editors
      neovim
      emacs29
      # shells
      fish
      zsh
      # shell utilities
      bat
      eza
      jq
      xsv
      rlwrap
      tree
      # network
      tailscale
      mullvad-vpn
      # wm
      sway
      (pass.withExtensions (ext: [ ext.pass-otp ]))
      rofi-wayland
      rofi-pass-wayland
      syncthing
    ];
  };
}
