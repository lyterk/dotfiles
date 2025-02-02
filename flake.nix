{
  description = "Home Manager configuration of lyterk";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      # nixpkgs-unstable,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    # pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
    {
      homeConfigurations."lyterk" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        # inherit pkgs-unstable;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];
        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
        # extraSpecialArgs.flakeInputs = [ doomEmacs ];
      };
    };
}
