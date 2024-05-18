{
  description = "Home Manager configuration of lyterk";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, git-doom-emacs, ... } @ inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      doomEmacs = pkgs.fetchFromGitHub {
        owner = "doomemacs";
	repo = "doomemacs";
	rev = "9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee";
      };
    in {
      homeConfigurations."lyterk" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ 
	  ./home.nix 
	];
        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
	extraSpecialArgs.flakeInputs = [ doomEmacs ];
      };
    };
}
