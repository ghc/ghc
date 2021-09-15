{ system }:

let
  sources = import ./nix/sources.nix;
  nixpkgsSrc = sources.nixpkgs;
  pkgs = import nixpkgsSrc { inherit system; };
in

let
  ghcVer = "ghc8107Binary";
  ghc = pkgs.haskell.compiler."${ghcVer}";
  alex = hsPkgs.alex;
  happy = hsPkgs.happy;
  hsPkgs = pkgs.haskellPackages;


  ourtexlive = with pkgs;
    texlive.combine {
      inherit (texlive)
        scheme-medium collection-xetex fncychap titlesec tabulary varwidth
        framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
    };
  fonts = with pkgs; makeFontsConf { fontDirectories = [ dejavu_fonts ]; };

  llvm = pkgs.llvm_11;
in

pkgs.writeTextFile {
  name = "toolchain";
  text = ''
    export PATH
    PATH="${pkgs.autoconf}/bin:$PATH"
    PATH="${pkgs.automake}/bin:$PATH"
    PATH="${pkgs.coreutils}/bin:$PATH"
    # Nixpkgs' ghc sets `C compiler command` to `clang`, which resolves to
    # /usr/bin/clang which targets the wrong platform. Bring nixpkgs' clang
    # into PATH to ensure that we use the correct target platform.
    # This is a temporary workaround for #20162.
    PATH="${pkgs.clang}/bin:$PATH"
    export FONTCONFIG_FILE=${fonts}
    export XELATEX="${ourtexlive}/bin/xelatex"
    export MAKEINDEX="${ourtexlive}/bin/makeindex"
    export HAPPY="${happy}/bin/happy"
    export ALEX="${alex}/bin/alex"
    export GHC="${ghc}/bin/ghc"
    export CC="${pkgs.clang_11}/bin/clang"
    export LLC="${llvm}/bin/llc"
    export OPT="${llvm}/bin/opt"
    export SPHINXBUILD="${pkgs.python3Packages.sphinx}/bin/sphinx-build"
    export CABAL_INSTALL="${pkgs.cabal-install}/bin/cabal"
    export CABAL="$CABAL_INSTALL"
    export CURSES_LIB_DIRS="${pkgs.ncurses}/lib"
  '';
}

