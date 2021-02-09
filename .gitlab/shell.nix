{ system ? "aarch64-darwin"
#, nixpkgs ? fetchTarball https://github.com/angerman/nixpkgs/archive/257cb120334.tar.gz #apple-silicon.tar.gz
, pkgs ? import <nixpkgs> { inherit system; }
}: pkgs.mkShell {
  # this prevents nix from trying to write the env-vars file.
  # we can't really, as NIX_BUILD_TOP/env-vars is not set.
  noDumpEnvVars=1;

  buildInputs = with pkgs; [
    haskell.compiler.ghc8103Binary
    haskell.packages.ghc8103Binary.cabal-install
    haskell.packages.ghc8103Binary.alex
    haskell.packages.ghc8103Binary.happy_1_19_12

    clang_11
    llvm_11

    automake
    autoconf

    gmp
    ncurses
    libiconv

    git

    python3
    # python3Full
    # python3Packages.sphinx

    which
    wget
    file

    cacert
  ];
}
