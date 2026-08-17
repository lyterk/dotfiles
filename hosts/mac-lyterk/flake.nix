{
  description = "Starter flake for a darwin-nix system";

  inputs = {
    # Nixpkgs for macOS
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    # nix-darwin for macOS system management
    # darwin.url = "github:LnL7/nix-darwin";
    darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    # Optional: home-manager for user configs
    # home-manager.url = "github:nix-community/home-manager";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Flake utils for convenience (optional)
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      darwin,
      flake-utils,
      home-manager,
      ...
    }:
    {
      # Darwin system configuration
      darwinConfigurations."mac-lyterk" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./configuration.nix
          # (Optional: add home-manager integration here later)
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.lyterk = import ./home.nix;
          }
        ];
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config = {
            allowUnfree = true;
          };
        };
      };
    };
}
