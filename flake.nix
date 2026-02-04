{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      # url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
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
      home-manager,
      sops-nix,
      ...
    }@flakeInputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
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
                inherit flakeInputs;
              };
              modules = [
                {
                  networking.hostName = name;
                  nixpkgs.overlays = [ (_: _: { nixfiles = self.packages.${system}; }) ];
                }
                home-manager.nixosModules.home-manager
                sops-nix.nixosModules.sops
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
