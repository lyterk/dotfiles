{
  pkgs,
  pkgs-unstable,
  config,
  ...
}:

let
  nightCity = pkgs.fetchurl {
    url = "https://getwallpapers.com/wallpaper/full/c/0/f/104160.jpg";
    sha256 = "sha256-01hpirLNkEaJ1zACu8Km1v2VG3H7Rgnx3zErOuufTbY=";
  };
  cityStreet = pkgs.fetchurl {
    url = "https://getwallpapers.com/wallpaper/full/a/6/c/35748.jpg";
    hash = "sha256-C9JT17w+jxnspAJN/Zr17SKAuMU4t2V1XZNhTCFtXWs=";
  };
in
{
  imports = [
    ../home-base.nix
    ../firefox
    ../waybar
    ../sway
    ../sway/gtklock.nix
  ];

  home.username = "work";
  home.homeDirectory = "/home/work";
  home.packages = with pkgs; [
    typescript-language-server
    aws-cdk-cli
    nodejs_22
    pnpm
    claude-code
    pkgs-unstable.code-cursor
    (python313.withPackages (
      ps: with ps; [
        pandas
        numpy
      ]
    ))
  ];
  home.file = {
    ".config/gtklock/config.ini".text = ''
      [main]
      gtk-theme=Adwaita-dark
      style=.config/gtklock/layout.css
    '';
    ".config/gtklock/layout.css".text = config.gtklock.mkGtklockCss nightCity;
  };

  nixpkgs.config.allowUnfree = true;

  home.stateVersion = "23.11"; # Please read the comment before changing.

  stylix = {
    enable = true;
    image = cityStreet;
  };

  programs = {
    fish = {
      enable = true;
      loginShellInit = ''
        if string match -q '/dev/tty2' (tty)
          exec sway
        end
      '';
    };

    keychain = {
      enable = true;
      enableFishIntegration = true;
      keys = [
        "~/.ssh/id_ed25519"
        "~/.ssh/work_ed25519"
      ];
    };

    git = {
      settings = {
        user.name = "Kevin Lyter";
        user.email = "kevin.lyter@yottanav.com";
        commit.gpgsign = true;
      };
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false;

      matchBlocks = {
        "gitlab.com" = {
          hostname = "gitlab.com";
          user = "git";
          identityFile = "~/.ssh/work_ed25519";
        };
      };
    };
  };

  # home.packages = with pkgs; [
  #   # code-cursor
  # ];
}
