{ system ? "aarch64-darwin"
, nixpkgs ? fetchTarball https://github.com/angerman/nixpkgs/archive/apple-silicon.tar.gz
, pkgs ? import nixpkgs { inherit system; }
}: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    haskell.compiler.ghc8103Binary
    haskell.packages.ghc8103Binary.cabal-install
    haskell.packages.ghc8103Binary.alex
    haskell.packages.ghc8103Binary.happy

    clang_11
    llvm_11

    automake
    autoconf

    gmp
    ncurses

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