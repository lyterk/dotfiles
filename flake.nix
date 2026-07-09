{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      # url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:nix-community/stylix/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO: Decide if this is worth the change
    # firefox-addons = {
    #   url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      sops-nix,
      stylix,
      ...
    }@flakeInputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      formatter.${system} = pkgs.nixpkgs-fmt;

      nixosConfigurations =
        let
          mkNixosConfiguration =
            name:
            nixpkgs.lib.nixosSystem {
              inherit system;
              specialArgs = {
                inherit flakeInputs pkgs-unstable;
              };
              modules = [
                {
                  networking.hostName = name;
                  nixpkgs.overlays = [ (_: _: { nixfiles = self.packages.${system}; }) ];
                }
                home-manager.nixosModules.home-manager
                sops-nix.nixosModules.sops
                # sops-nix.homeManagerModules.sops
                stylix.nixosModules.stylix
                # stylix.homeManagerModules.stylix
                ./shared
                (./hosts + "/${name}" + /configuration.nix)
                (./hosts + "/${name}" + /hardware.nix)
              ];
            };
        in
        {
          laptop = mkNixosConfiguration "laptop";
          desktop = mkNixosConfiguration "desktop";
          miranda = mkNixosConfiguration "miranda";
        };

      apps.${system} =
        let
          mkApp = name: script: {
            type = "app";
            program = toString (pkgs.writeShellScript "${name}.sh" script);
          };
        in
        {
          fmt = mkApp "fmt" ''
            PATH=${with pkgs; lib.makeBinPath [ nixfmt-rfc-style ]}

            ${pkgs.lib.fileContents ./scripts/fmt.sh}
          '';

          lint = mkApp "lint" ''
            # TODO: add nix-linter back when the package is no longer broken
            PATH=${with pkgs; lib.makeBinPath [ statix ]}

            ${pkgs.lib.fileContents ./scripts/lint.sh}
          '';
        };
    };
}
