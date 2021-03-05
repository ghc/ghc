{ system ? "aarch64-darwin"
#, nixpkgs ? fetchTarball https://github.com/angerman/nixpkgs/archive/257cb120334.tar.gz #apple-silicon.tar.gz
, pkgs ? import <nixpkgs> { inherit system; }
, compiler ? if system == "aarch64-darwin" then "ghc8103Binary" else "ghc8103"
}: pkgs.mkShell {
  # this prevents nix from trying to write the env-vars file.
  # we can't really, as NIX_BUILD_TOP/env-vars is not set.
  noDumpEnvVars=1;

  # stop polluting LDFLAGS with -liconv
  dontAddExtraLibs = true;

  # we need to inject ncurses into --with-curses-libraries.
  # the real fix is to teach terminfo to use libcurses on macOS.
  CONFIGURE_ARGS = "--with-intree-gmp --with-curses-libraries=${pkgs.ncurses.out}/lib";

  buildInputs = (with pkgs; [
    haskell.compiler.${compiler}
    haskell.packages.${compiler}.cabal-install
    haskell.packages.${compiler}.alex
    haskell.packages.${compiler}.happy # _1_19_12 is needed for older GHCs.

    clang_11
    llvm_11

    automake
    autoconf
    m4

    gmp
    ncurses
    libiconv
    zlib.out
    zlib.dev
    glibcLocales
    # locale doesn't build yet :-/
    # locale

    git

    python3
    # python3Full
    # python3Packages.sphinx
    perl

    which
    wget
    file

    xz
    xlibs.lndir

    cacert ])
  ++ (with pkgs.darwin.apple_sdk.frameworks; [ Foundation Security ]);
}
