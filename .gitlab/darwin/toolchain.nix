{ system }:

let
  sources = import ./nix/sources.nix;
  nixpkgsSrc = sources.nixpkgs;
  pkgs = import nixpkgsSrc { inherit system; };
in

let
  hsPkgs = pkgs.haskellPackages;
  alex = hsPkgs.alex;
  happy = hsPkgs.happy;
  targetTriple = pkgs.stdenv.targetPlatform.config;

  ghcBindists = let version = ghc.version; in {
    aarch64-darwin = pkgs.fetchurl {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-aarch64-apple-darwin.tar.xz";
      sha256 = "sha256:0p2f35pihlnmkm7x73b5xm3dyhiczrywc19khr7i7vb2q1y4zw6i";
    };
    x86_64-darwin = pkgs.fetchurl {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-x86_64-apple-darwin.tar.xz";
      sha256 = "sha256:0gzq0vfjbhr9n8z63capvdwrw7bisy15d5c1y1gynfix13bbnjlk";
    };
  };

  ghc = pkgs.stdenv.mkDerivation rec {
    version = "9.2.2";
    name = "ghc";
    src = ghcBindists.${pkgs.stdenv.hostPlatform.system};
    configureFlags = [
      "CC=/usr/bin/clang"
      "CLANG=/usr/bin/clang"
      "LLC=${llvm}/bin/llc"
      "OPT=${llvm}/bin/opt"
      "CONF_CC_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_CXX_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_GCC_LINKER_OPTS_STAGE2=--target=${targetTriple}"
    ];
    buildPhase = "true";

    # N.B. Work around #20253.
    nativeBuildInputs = [ pkgs.gnused ];
    postInstallPhase = ''
      settings="$out/lib/ghc-${version}/settings"
      sed -i -e "s%\"llc\"%\"${llvm}/bin/llc\"%" $settings
      sed -i -e "s%\"opt\"%\"${llvm}/bin/opt\"%" $settings
      sed -i -e "s%\"clang\"%\"/usr/bin/clang\"%" $settings
      sed -i -e 's%("C compiler command", "")%("C compiler command", "/usr/bin/clang")%' $settings
      sed -i -e 's%("C compiler flags", "")%("C compiler flags", "--target=${targetTriple}")%' $settings
      sed -i -e 's%("C++ compiler flags", "")%("C++ compiler flags", "--target=${targetTriple}")%' $settings
      sed -i -e 's%("C compiler link flags", "")%("C compiler link flags", "--target=${targetTriple}")%' $settings
    '';

    # Sanity check: verify that we can compile hello world.
    doInstallCheck = true;
    installCheckPhase = ''
      unset DYLD_LIBRARY_PATH
      $out/bin/ghc --info
      cd $TMP
      mkdir test-ghc; cd test-ghc
      cat > main.hs << EOF
        {-# LANGUAGE TemplateHaskell #-}
        module Main where
        main = putStrLn \$([|"yes"|])
      EOF
      $out/bin/ghc --make -v3 main.hs || exit 1
      echo compilation ok
      [ $(./main) == "yes" ]
    '';
  };

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
    export FONTCONFIG_FILE=${fonts}
    export XELATEX="${ourtexlive}/bin/xelatex"
    export MAKEINDEX="${ourtexlive}/bin/makeindex"
    export HAPPY="${happy}/bin/happy"
    export ALEX="${alex}/bin/alex"
    export GHC="${ghc}/bin/ghc"
    export LLC="${llvm}/bin/llc"
    export OPT="${llvm}/bin/opt"
    export SPHINXBUILD="${pkgs.python3Packages.sphinx}/bin/sphinx-build"
    export CABAL_INSTALL="${pkgs.cabal-install}/bin/cabal"
    export CABAL="$CABAL_INSTALL"

    sdk_path="$(xcrun --sdk macosx --show-sdk-path)"
    export CONFIGURE_ARGS="$CONFIGURE_ARGS --with-ffi-libraries=$sdk_path/usr/lib --with-ffi-includes=$sdk_path/usr/include/ffi"
  '';
}
