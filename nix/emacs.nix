{
  description "Flake for cloning and building Doom Emacs, with my personal configuration";

  outputs = { self, nixpkgs }: {
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        doomSrc = pkgs.fetchFromGithub {
	  owner = "doomemacs";
	  repo = "doomemacs";
	  rev = "9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee";
	};
	doomConfigSrc = builtins.fetchGit {
	  url = "git@txru.me:/git/doomd.git";
	};
	doomExecutable = "${doomSrc}/bin/doom";
      in {
	stdenv.mkDerivation {
	  name = "doomInstall";
	  src = doomSrc;
	  buildInputs = [ pkgs.emacs29 ];
	  buildPhase = "${doomExecutable} install
      }
    };
  };
}
