{ pkgs, ... }:

let
  eagleTreesFog = pkgs.fetchurl {
    url = "https://getwallpapers.com/wallpaper/full/1/8/2/74758.jpg";
    sha256 = "sha256-IsUUXSdKv5sZsg0sMKF3e+ldB02o7I5Tvk3HIMD9/p4=";
  };
  milkyWay = pkgs.fetchurl {
    # pastoral scene
    # url = "https://getwallpapers.com/wallpaper/full/c/7/2/454788.jpg";
    # hash = "sha256-3jJ1KZ3zxW9sb2q9n3foMMv7Ey78Gl5c43EC7fPQKtc=";
    url = "https://getwallpapers.com/wallpaper/full/a/b/1/126669.jpg";
    hash = "sha256-vxGCjrK1nYsMAQSfeJCb37j4/snuoK367gNw/GemAiw=";
  };
in
{
  imports = [
    # ./nix/batteryNotifier.nix
    # ./nix/unstables/flake.nix
    ../../../shared/home-base.nix
    ../../../shared/firefox
    ../../../shared/waybar
    ../../../shared/sway
  ];
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "lyterk";
  home.homeDirectory = "/home/lyterk";
  home.file = {
    ".config/gtklock/config.ini".text = ''
      [main]
      gtk-theme=Adwaita-dark
      style=.config/gtklock/layout.css
    '';
    ".config/gtklock/layout.css".text = ''
      window {
         background-image: url("${eagleTreesFog}");
         background-size: cover;
         background-repeat: no-repeat;
         background-position: center;
         background-color: gray;
         color: white;
      }
    '';
  };

  stylix = {
    enable = true;
    image = milkyWay;
  };

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.

  programs = {
    fish = {
      enable = true;
      loginShellInit = ''
        if string match -q '/dev/tty1' (tty)
          exec sway
        end
      '';
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
        bitbucket = {
          hostname = "bitbucket.org";
          user = "git";
          identityFile = "~/.ssh/id_ed25519";
        };
        desktop = {
          hostname = "txru.me";
          port = 65222;
          user = "lyterk";
          identityFile = "~/.ssh/id_ed25519";
        };
        remarkable = {
          hostname = "10.11.99.1";
          user = "root";
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
}
