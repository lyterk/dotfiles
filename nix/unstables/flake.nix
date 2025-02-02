{
  description = "Packages that need the unstable channel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # doomEmacs.url = "github:doomemacs/doomemacs?rev=";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
      in
      with pkgs;
      {
        devShell = mkShell { buildInputs = [ zed-editor ]; };
      }
    );
}
