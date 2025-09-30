{pkgs ? import <nixpkgs> {}, ...}: let
  f = {
    mkDerivation,
    base,
    brick,
    containers,
    lib,
    vty,
    vty-unix,
  }:
    mkDerivation {
      pname = "tuidui";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [base brick containers vty vty-unix];
      license = lib.licenses.bsd3;
      mainProgram = "tuidui";
    };
in (pkgs.haskellPackages.callPackage f {})
