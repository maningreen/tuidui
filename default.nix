{
  pkgs ? import <nixpkgs> { },
}:
let
  lib = pkgs.lib;
  hsPkgs = pkgs.haskellPackages;
in
hsPkgs.mkDerivation {
  pname = "tuidui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = with hsPkgs; [
    base
    brick
    containers
    vty
    vty-unix
  ];
  license = lib.licenses.bsd3;
  mainProgram = "tuidui";
}