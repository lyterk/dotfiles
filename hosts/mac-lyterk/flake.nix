{
  description = "Starter flake for a darwin-nix system";

  inputs = {
    # Nixpkgs for macOS
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    # nix-darwin for macOS system management
    # darwin.url = "github:LnL7/nix-darwin";
    darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
    # Optional: home-manager for user configs
    # home-manager.url = "github:nix-community/home-manager";

    # Flake utils for convenience (optional)
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, darwin, flake-utils, ... }:
      {
        # Darwin system configuration
        darwinConfigurations."mac-lyterk" = darwin.lib.darwinSystem {
	  system = "aarch64-darwin";
          modules = [
            ./configuration.nix
            # (Optional: add home-manager integration here later)
          ];
	pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config = { allowUnfree = true; };
        };
        };
      };
}
