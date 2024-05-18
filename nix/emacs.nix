{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-doom-emacs,
    ...
  }: {
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system  = "x86_64-linux";
      modules = [
        home-manager.nixosModules.home-manager
        {
          home-manager.users.exampleUser = { ... }: {
            imports = [ nix-doom-emacs.hmModule ];
            programs.doom-emacs = {
              enable = true;
              doomPrivateDir = .config/doom; 
            };
          };
        }
      ];
    };
  };
}
