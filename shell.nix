# Invoking nix-shell sets up an environment where we can build ghc
# by only invoking hadrian.


{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskell.packages.ghc821;

  removeBuild = path: type:
    let baseName = baseNameOf (toString path);
    in
        ! (baseName == "_build"
           || baseName == "dist"
           || baseName == "dist-newstyle"
           || baseName == ".stack-work"
           || baseName == "config.log"
           || baseName == "config.status"
           || nixpkgs.lib.hasSuffix ".sh" baseName
           || !(nixpkgs.lib.cleanSourceFilter path type)) ;

  filterSrc = path: builtins.filterSource removeBuild path;


  hadrianPackages = nixpkgs.haskell.packages.ghc821.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {};
      in {
        hadrian = localPackage "hadrian" ./. ;
        shake = self.callHackage "shake" "0.16" {};
        Cabal = localPackage "Cabal" ./../libraries/Cabal/Cabal ;
        filepath = localPackage "filepath" ./../libraries/filepath ;
        text = localPackage "text" ./../libraries/text  ;
        hpc = localPackage"hpc" ./../libraries/hpc ;
        parsec = localPackage "parsec" ./../libraries/parsec ;
        HUnit = nixpkgs.haskell.lib.dontCheck (self.callHackage "HUnit" "1.3.1.2" {});
        process = localPackage "process" ./../libraries/process ;
        directory = localPackage "directory" ./../libraries/directory ;
      }; };

in

  nixpkgs.stdenv.mkDerivation {
    name = "ghc-dev";
    buildInputs = [
                    hadrianPackages.hadrian
                    nixpkgs.haskell.compiler.ghc821
                    haskellPackages.alex
                    haskellPackages.happy
                    nixpkgs.python3
                    nixpkgs.git
                    nixpkgs.autoconf
                    nixpkgs.automake
                    nixpkgs.perl
                    nixpkgs.gcc
                    nixpkgs.python3Packages.sphinx
                    nixpkgs.ncurses
                    nixpkgs.m4
                    nixpkgs.gmp
                    nixpkgs.file ];
  }
