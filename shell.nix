# Invoking nix-shell sets up an environment where we can build ghc
# by only invoking hadrian.


{ _nixpkgs ? import <nixpkgs> {} }:

let

  nixpkgs = import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "e7a327da5cffdf5e77e1924906a4f0983591bd3e";
    sha256 = "1xzil4mayhggg2miwspbk12nihlszg0y4n6i4qacrxql5n75f0hr";
  }){ overlays = [cabalHashes]; };



  cabalHashes = sel: super: {
    all-cabal-hashes = super.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b2b93ae610f5f1b51d22b191f972dc3dec8f94c6.tar.gz";
      sha256 = "0bffclpqbw62xff36qlzxghr042mhv0m06k5ml4298w6fv7ly1xw";
    };
  };

  haskellPackages = nixpkgs.haskell.packages.ghc822;

  removeBuild = path: type:
    let baseName = baseNameOf (toString path);
    in
        ! (baseName == "_build"
           || baseName == "dist"
           || baseName == "dist-newstyle"
           || baseName == ".stack-work"
           || baseName == "config.log"
           || baseName == "config.status"
           || baseName == "shell.nix"
           || nixpkgs.lib.hasSuffix ".sh" baseName
           || !(nixpkgs.lib.cleanSourceFilter path type)) ;

  filterSrc = path: builtins.filterSource removeBuild path;


  hadrianPackages = nixpkgs.haskell.packages.ghc822.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {};
      in {
        hadrian = localPackage "hadrian" ./. ;
        happy = nixpkgs.haskell.lib.dontCheck (super.happy);
        shake = self.callHackage "shake" "0.16.2" {};
        extra = self.callHackage "extra" "1.6.4" {};
        QuickCheck = self.callHackage "QuickCheck" "2.10" {};
        Cabal = localPackage "Cabal" ./../libraries/Cabal/Cabal ;
        filepath = localPackage "filepath" ./../libraries/filepath ;
        text = localPackage "text" ./../libraries/text  ;
        hpc = localPackage"hpc" ./../libraries/hpc ;
        parsec = localPackage "parsec" ./../libraries/parsec ;
        HUnit = nixpkgs.haskell.lib.dontCheck (self.callHackage "HUnit" "1.3.1.2" {});
        process = localPackage "process" ./../libraries/process ;
        directory = localPackage "directory" ./../libraries/directory ;
      }; };

  cabalPackages = nixpkgs.haskell.packages.ghc822.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {};
      in {
        Cabal = localPackage "Cabal" ./../../cabal/Cabal ;
        cabal-install = self.callPackage ./../../cabal/cabal-install.nix {};
      }; };


in
  nixpkgs.lib.overrideDerivation nixpkgs.haskell.packages.ghcHEAD.ghc
    (drv: {
      name = "ghc-dev";
      nativeBuildInputs = drv.nativeBuildInputs ++
        [ hadrianPackages.hadrian
          nixpkgs.arcanist
          nixpkgs.git
          nixpkgs.gmp.dev nixpkgs.gmp.out
          nixpkgs.ncurses.dev nixpkgs.ncurses.out
          nixpkgs.python3Packages.sphinx
          nixpkgs.texlive.combined.scheme-basic
          (nixpkgs.haskell.packages.ghc822.ghcWithPackages
            (ps: [ps.html ps.regex-compat ps.dump-core]))

          #cabalPackages.cabal-install
        ];
    })

