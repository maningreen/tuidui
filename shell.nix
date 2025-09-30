{
  pkgs ? import <nixpkgs> {},
  compiler ? "default",
  doBenchmark ? false,
}: let

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

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  variant =
    if doBenchmark
    then pkgs.haskell.lib.doBenchmark
    else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in
  if pkgs.lib.inNixShell
  then drv.env
  else drv
