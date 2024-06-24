{ system }:

let
  sources = import ./nix/sources.nix;
  nixpkgsSrc = sources.nixpkgs;
  pkgs = import nixpkgsSrc { inherit system; };
  hostPkgs = import nixpkgsSrc { };
in

let
  hsPkgs = pkgs.haskellPackages;
  alex = hsPkgs.alex;
  happy = hsPkgs.happy;
  targetTriple = pkgs.stdenv.targetPlatform.config;

  ghcBindists = let version = ghc.version; in {
    aarch64-darwin = hostPkgs.fetchurl {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-aarch64-apple-darwin.tar.xz";
      sha256 = "sha256-c1GTMJf3/yiW/t4QL532EswD5JVlgA4getkfsxj4TaA=";
    };
    x86_64-darwin = hostPkgs.fetchurl {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-x86_64-apple-darwin.tar.xz";
      sha256 = "sha256-LrYniMG0phsvyW6dhQC+3ompvzcxnwAe6GezEqqzoTQ=";
    };

  };

  ghc = pkgs.stdenv.mkDerivation rec {
    # Using 9.6.2 because of #24050
    version = "9.6.2";
    name = "ghc";
    src = ghcBindists.${pkgs.stdenv.hostPlatform.system};
    configureFlags = [
      "CC=/usr/bin/clang"
      "CLANG=/usr/bin/clang"
      "AR=/usr/bin/ar"
      "LLC=${llvm}/bin/llc"
      "OPT=${llvm}/bin/opt"
      "LLVMAS=${llvm_clang}/bin/clang"
      "CONF_CC_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_CXX_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_GCC_LINKER_OPTS_STAGE2=--target=${targetTriple}"
    ];
    buildPhase = "true";

    # This is a horrible hack because the configure script invokes /usr/bin/clang
    # without a `--target` flag. Then depending on whether the `nix` binary itself is
    # a native x86 or arm64 binary means that /usr/bin/clang thinks it needs to run in
    # x86 or arm64 mode.

    # The correct answer for the check in question is the first one we try, so by replacing
    # the condition to true; we select the right C++ standard library still.
    preConfigure = ''
      sed "s/\"\$CC\" -o actest actest.o \''${1} 2>\/dev\/null/true/i" configure > configure.new
      mv configure.new configure
      chmod +x configure
      cat configure

    '';

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

  llvm = pkgs.llvm_15;
  llvm_clang = pkgs.llvmPackages_15.clang-unwrapped;
in
pkgs.writeTextFile {
  name = "toolchain";
  text = ''
    export PATH
    PATH="${pkgs.autoconf}/bin:$PATH"
    PATH="${pkgs.automake}/bin:$PATH"
    export FONTCONFIG_FILE=${fonts}
    export XELATEX="${ourtexlive}/bin/xelatex"
    export MAKEINDEX="${ourtexlive}/bin/makeindex"
    export HAPPY="${happy}/bin/happy"
    export ALEX="${alex}/bin/alex"
    export GHC="${ghc}/bin/ghc"
    export LLC="${llvm}/bin/llc"
    export OPT="${llvm}/bin/opt"
    export LLVMAS="${llvm_clang}/bin/clang"
    export SPHINXBUILD="${pkgs.python3Packages.sphinx}/bin/sphinx-build"
    export CABAL_INSTALL="${pkgs.cabal-install}/bin/cabal"
    export CABAL="$CABAL_INSTALL"

    sdk_path="$(xcrun --sdk macosx --show-sdk-path)"
    : ''${CONFIGURE_ARGS:=}
    CONFIGURE_ARGS+="''${CONFIGURE_ARGS:+ }--with-ffi-libraries=$sdk_path/usr/lib --with-ffi-includes=$sdk_path/usr/include/ffi --build=${targetTriple}"
    export CONFIGURE_ARGS
  '';
}
