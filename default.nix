# Taken from https://github.com/alpmestan/ghc.nix
#
# Usage examples:
#
#   nix-shell path/to/ghc.nix/ --pure --run './boot && ./configure && make -j4'
#   nix-shell path/to/ghc.nix/        --run 'hadrian/build -c -j4 --flavour=quickest'
#   nix-shell path/to/ghc.nix/        --run 'THREADS=4 ./validate --slow'
#
let
  fetchNixpkgs = import ./nix/fetch-tarball-with-override.nix "custom_nixpkgs";
  fetchGhcIde  = import ./nix/fetch-tarball-with-override.nix "ghcide";
in
{ nixpkgsPin ? ./nix/pins/nixpkgs.src-json
, nixpkgs   ? import (fetchNixpkgs nixpkgsPin) {}
, bootghc   ? "ghc882"
, version   ? "8.9"
, hadrianCabal ? (builtins.getEnv "PWD") + "/hadrian/hadrian.cabal"
, useClang  ? false  # use Clang for C compilation
, withLlvm  ? false
, withDocs  ? true
, withGhcid ? false
# GHCIDE support on hold as we must use GHC 8.8 minimum for GHC development, and GHCIDE is not yet available for GHC 8.8
# See https://github.com/cachix/ghcide-nix/issues/3
# See https://github.com/alpmestan/ghc.nix/issues/64
# , withIde   ? false
, withHadrianDeps ? false
, withDwarf  ? nixpkgs.stdenv.isLinux  # enable libdw unwinding support
, withNuma   ? nixpkgs.stdenv.isLinux
, withDtrace ? nixpkgs.stdenv.isLinux
, withGrind ? true
, cores     ? 4
}:

with nixpkgs;

let
    llvmForGhc = llvm_7;

    stdenv =
      if useClang
      then nixpkgs.clangStdenv
      else nixpkgs.stdenv;
    noTest = pkg: haskell.lib.dontCheck pkg;

    hspkgs = haskell.packages.${bootghc};

    ghcide-src = fetchGhcIde ./nix/pins/ghcide-nix.src-json ;

    ghcide = (import ghcide-src {})."ghcide-${bootghc}";

    ghc    = haskell.compiler.${bootghc};

    ourtexlive =
      nixpkgs.texlive.combine {
        inherit (nixpkgs.texlive)
          scheme-medium collection-xetex fncychap titlesec tabulary varwidth
          framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
      };
    fonts = nixpkgs.makeFontsConf { fontDirectories = [ nixpkgs.dejavu_fonts ]; };
    docsPackages = if withDocs then [ python3Packages.sphinx ourtexlive ] else [];

    depsSystem = with stdenv.lib; (
      [ autoconf automake m4 less
        gmp.dev gmp.out glibcLocales
        ncurses.dev ncurses.out
        perl git file which python3
        xlibs.lndir  # for source distribution generation
        zlib.out
        zlib.dev
      ]
      ++ docsPackages
      ++ optional withLlvm llvmForGhc
      ++ optional withGrind valgrind
      ++ optional withNuma numactl
      ++ optional withDwarf elfutils
      ++ optional withGhcid ghcid
      # ++ optionals withIde [ghcide ccls bear]
      ++ optional withDtrace linuxPackages.systemtap
      ++ (if (! stdenv.isDarwin)
          then [ pxz ]
          else [
            libiconv
            darwin.libobjc
            darwin.apple_sdk.frameworks.Foundation
          ])
    );
    depsTools = with hspkgs; [ alex cabal-install happy ];

    hadrianCabalExists = builtins.pathExists hadrianCabal;
    hsdrv = if (withHadrianDeps &&
                builtins.trace "checking if ${toString hadrianCabal} is present:  ${if hadrianCabalExists then "yes" else "no"}"
                hadrianCabalExists)
            then hspkgs.callCabal2nix "hadrian" hadrianCabal {}
            else (hspkgs.mkDerivation rec {
              inherit version;
              pname   = "ghc-buildenv";
              license = "BSD";
              src = builtins.filterSource (_: _: false) ./.;

              libraryHaskellDepends = with hspkgs; lib.optionals withHadrianDeps [
                extra
                QuickCheck
                shake
                unordered-containers
              ];
              librarySystemDepends = depsSystem;
            });
in
(hspkgs.shellFor rec {
  packages    = pkgset: [ hsdrv ];
  nativeBuildInputs = depsTools;
  buildInputs = depsSystem;

  hardeningDisable    = ["fortify"]                  ; ## Effectuated by cc-wrapper
  # Without this, we see a whole bunch of warnings about LANG, LC_ALL and locales in general.
  # In particular, this makes many tests fail because those warnings show up in test outputs too...
  # The solution is from: https://github.com/NixOS/nix/issues/318#issuecomment-52986702
  LOCALE_ARCHIVE      = if stdenv.isLinux then "${glibcLocales}/lib/locale/locale-archive" else "";
  CONFIGURE_ARGS      = [ "--with-gmp-includes=${gmp.dev}/include"
                          "--with-gmp-libraries=${gmp}/lib"
                        ] ++ lib.optionals withNuma [
                          "--with-libnuma-includes=${numactl}/include"
                          "--with-libnuma-libraries=${numactl}/lib"
                        ] ++ lib.optionals withDwarf [
                          "--with-libdw-includes=${elfutils}/include"
                          "--with-libdw-libraries=${elfutils}/lib"
                          "--enable-dwarf-unwind"
                        ];

  shellHook           = let toYesNo = b: if b then "YES" else "NO"; in ''
    # somehow, CC gets overriden so we set it again here.
    export CC=${stdenv.cc}/bin/cc
    export HAPPY=${hspkgs.happy}/bin/happy
    export ALEX=${hspkgs.alex}/bin/alex
    ${lib.optionalString withLlvm "export LLC=${llvmForGhc}/bin/llc"}
    ${lib.optionalString withLlvm "export OPT=${llvmForGhc}/bin/opt"}

    # "nix-shell --pure" resets LANG to POSIX, this breaks "make TAGS".
    export LANG="en_US.UTF-8"
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${lib.makeLibraryPath depsSystem}"
    unset LD

    ${lib.optionalString withDocs "export FONTCONFIG_FILE=${fonts}"}

    # A convenient shortcut
    configure_ghc() { ./configure $CONFIGURE_ARGS $@; }

    echo "Recommended ./configure arguments (found in \$CONFIGURE_ARGS:"
    echo "or use the configure_ghc command):"
    echo ""
    echo "  ${lib.concatStringsSep "\n  " CONFIGURE_ARGS}"
  '';
})
