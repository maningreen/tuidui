# tuidui

Tuidui is a small per-directory todo app written in haskell with the bricks library

## Usage

When running, you will see a menu, type `?` or `h` to see a list of available commands
Tuidui is a modal editor, with vim-style bindings, modes are:
Help, Insert, and Normal

When you quit the program all items will be writen to `./todo`, one line per item. Example:
```
FizzBuzz
Figure out what "Fizzing is"
```
When you have too many items to do, and not enough of space to see it, you should probably get going! Or you could just select down with `j` and the view will scroll with you.

## Build/Install Guide

### Cabal

In order to build with cabal:
```bash
cabal run
```
This will automatically fetch all the dependancies, build the program and run it

In order to install with cabal:
```bash
cabal install
```
This will automatically compile it and it's dependancies, and add a symlink into `~/.local/bin`

### Nix

In order to build with nix:

```bash
nix-build
```

To develop:
```bash 
nix-shell
```

Or with flakes:
```bash
nix build
```

```bash 
nix develop
```

In order to install with NixOS, add this to your configuration, or something equal to it
```nix
{ pkgs, ... }: {
    environment.systemPackage = [
        (pkgs.fetchFromGitHub {
            owner = "maningreen";
            repo = "tuidui";
            rev = ""; # replace the empty string with a git commit hash or use tag
            tag = ""; # replace the empty string with a git tag, or use rev
        })
    ]
}
```
Then rebuild with `sudo nixos-rebuild switch --flake /path/to/your/flake`

### Nix Flakes

In order to install with nix flakes, add this to your flake's inputs
```nix
{
    inputs = {
        tuidui = {
            url = "github:maningreen/tuidui"
            # it's recomended that you set this to be the name of your nixpkgs
            inputs.nixpkgs.follows = "nixpkgs"
        };
    };
}
```

If you have `outputs = inputs@{ nixpkgs, ... }:` or `outputs = inputs:` then add this to your nixos system config
if you have `outputs = { nixpkgs, ... }:`, then ensure you define `tuidui` as an input to the output function

```nix
{
    # functionally inputs: and inputs@{ nixpkgs , ...}: are the same here
    #                           only add this if you don't have @inputs
    outputs = inputs@{ nixpkgs, tuidui, ... }: {
        nixosConfigurations = {
            # replace this with your hostname
            nixos = nixpkgs.lib.nixosSystem {
                # replace this with your system, chances are it's "x86_64-linux"
                system = "x86_64-linux"
                specialArgs = { inherit inputs; };
                # or `specialArgs = { inherit tuidui; };` if you don't have @inputs
                modules = [
                    # replace this with the relative path to your configuration.nix
                    ./configuration.nix
                    # if you don't want to define a function in your flake, an alternative is to put the contents of the return
                    # in your configuration.nix
                    ({ pkgs, tuidui, inputs, ... }: {
                        #    ^ only if you don't have @inputs
                        environment.systemPackage = [
                            inputs.tuidui.packages.${pkgs.system}.default
                            # or if you don't have @inputs
                            tuidui.packages.${pkgs.system}.default
                        ]
                    })
                ];
            };
        };
    }
}
```
Then rebuild with `sudo nixos-rebuild switch --flake /path/to/your/flake`
If you want to install them via home-manager (We'll assume you have `inputs@`)

```nix
{
    outputs = inputs@{ nixpkgs, ... }: {
        homeConfigurations = {
            # replace this with username@hostname
            "alice@nixos" = inputs.home-manager.lib.homeManagerconfiguration {
                extraSpecialArgs = { inherit inputs; };
                # pkgs = ...
                modules = [
                    ./home-configuration.nix
                    # or your path to it
                    # if you don't want to  define a function in your flake, an alternative would be to put the return of it 
                    in ./home-configuration.nix
                    ({ pkgs, inputs, ... }: {
                        home.packages = [
                            inputs.tuidui.packages.${pkgs.system}.default
                        ]
                    })
                ];
            };
        };
    }
}
```
Then rebuild with `home-manager switch --flake /path/to/your/flake`
