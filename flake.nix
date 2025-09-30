{
  description = "Tuidui: a simple tui based to do app written in haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, self , ...}: let
    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    forAllSystems = nixpkgs.lib.genAttrs systems;
  in {
    devShells = forAllSystems (s: let
      pkgs = nixpkgs.legacyPackages.${s};
    in {
      default = self.devShells.${s}.tuidui;
      tuidui = pkgs.callPackage ./shell.nix { inherit pkgs; };
    });

    packages = forAllSystems (s: let
      pkgs = nixpkgs.legacyPackages.${s};
    in {
      default = self.packages.${s}.tuidui;
      tuidui = pkgs.callPackage ./default.nix { inherit pkgs; };
    });

  };
}
