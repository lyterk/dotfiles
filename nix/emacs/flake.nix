{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-23.11";
    # doomEmacs.url = "github:doomemacs/doomemacs?rev=";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    doomSrc = pkgs.fetchFromGitHub {
      owner = "doomemacs";
      repo = "doomemacs";
      rev = "9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee";
      sha256 = "sha256-Fhir4WlcfEh70V8+oNS1LVAGBftiqtD2qaHzOC8BJUI=";
    };
    doomEmacs = pkgs.stdenv.mkDerivation {
      name = "doomEmacs";
      src = doomSrc;
      buildInputs = [
        pkgs.emacs29
        pkgs.git
        (pkgs.ripgrep.override { withPCRE2 = true; })
      ];
      buildPhase = "${doomSrc}/bin/doom install";
    };
  in {
    packages = {
      "${system}" = {
         doomEmacs = doomEmacs;
	 # default = doomEmacs;
      };
    };
    defaultPackage."${system}" = doomEmacs;
  };
}
