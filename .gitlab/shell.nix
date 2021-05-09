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
  # CONFIGURE_ARGS = "--with-intree-gmp --with-curses-libraries=${pkgs.ncurses.out}/lib";
  CONFIGURE_ARGS = "--with-intree-gmp --with-curses-libraries=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib --with-iconv-includes=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include --with-iconv-libraries=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib SH=/bin/bash";

  # magic speedup pony :facepalm:
  #
  # nix has the ugly habbit of duplicating ld flags more than necessary.  This
  # somewhat consolidates this.
  shellHook = ''
  export NIX_LDFLAGS=$(for a in $NIX_LDFLAGS; do echo $a; done |sort|uniq|xargs)
  export NIX_LDFLAGS_FOR_TARGET=$(for a in $NIX_LDFLAGS_FOR_TARGET; do echo $a; done |sort|uniq|xargs)
  export NIX_LDFLAGS_FOR_TARGET=$(comm -3 <(for l in $NIX_LDFLAGS_FOR_TARGET; do echo $l; done) <(for l in $NIX_LDFLAGS; do echo $l; done))


  # Impurity hack for GHC releases.
  #################################
  # We don't want binary releases to depend on nix, thus we'll need to make sure we don't leak in references.
  # GHC externally depends only on iconv and curses.  However we can't force a specific curses library for
  # the terminfo package, as such we'll need to make sure we only look in the system path for the curses library
  # and not pick up the tinfo from the nix provided ncurses package.
  #
  # We also need to force us to use the systems COREFOUNDATION, not the one that nix builds. Again this is impure,
  # but it will allow us to have proper binary distributions.
  #
  # do not use nixpkgs provided core foundation
  export NIX_COREFOUNDATION_RPATH=/System/Library/Frameworks
  # drop curses from the LDFLAGS, we really want the system ones, not the nix ones.
  export NIX_LDFLAGS=$(for lib in $NIX_LDFLAGS; do case "$lib" in *curses*);; *) echo -n "$lib ";; esac; done;)
  export NIX_CFLAGS_COMPILE+=" -Wno-nullability-completeness -Wno-availability -Wno-expansion-to-defined -Wno-builtin-requires-header -Wno-unused-command-line-argument"

  # unconditionally add the MacOSX.sdk and TargetConditional.h
  export NIX_CFLAGS_COMPILE+=" -isystem /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
  '';

  nativeBuildInputs = (with pkgs; [
    # This needs to come *before* ghc,
    # otherwise we migth end up with the clang from
    # the bootstrap GHC in PATH with higher priority.
    clang_11
    llvm_11

    haskell.compiler.${compiler}
    haskell.packages.${compiler}.cabal-install
    haskell.packages.${compiler}.alex
    haskell.packages.${compiler}.happy_1_19_12

    automake
    autoconf
    m4

    gmp
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
