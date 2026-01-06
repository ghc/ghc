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
  targetTriple = pkgs.stdenvNoCC.targetPlatform.config;

  ghcBindists = let version = ghc.version; in {
    aarch64-darwin = hostPkgs.fetchzip {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-aarch64-apple-darwin.tar.xz";
      hash = "sha512-xUlt7zc/OT3a1SR0BxmFFgrabPkWUENATdw4NbQwEi5+nH5yPau+HSrGI5UUoKdO4gdpgZlPaxtI7eSk0fx1+g==";
    };
    x86_64-darwin = hostPkgs.fetchzip {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-x86_64-apple-darwin.tar.xz";
      hash = "sha512-4/INeJwPPGbOj9MepwnIvIg2lvFkqS8w/3U/I8f6gCsoNlgwPr78iyY9vd6vfMONR1GxNQU3L/lxE07F3P0Qag==";
    };
  };

  ghc = pkgs.stdenvNoCC.mkDerivation rec {
    version = "9.10.3";
    name = "ghc";
    src = ghcBindists.${pkgs.stdenvNoCC.hostPlatform.system};

    dontUpdateAutotoolsGnuConfigScripts = true;

    configureFlags = [
      "AR=/usr/bin/ar"
      "CC=/usr/bin/clang"
      "CXX=/usr/bin/clang++"
      "INSTALL=/usr/bin/install"
      "INSTALL_NAME_TOOL=/usr/bin/install_name_tool"
      "MergeObjsCmd=/usr/bin/ld"
      "NM=/usr/bin/nm"
      "OTOOL=/usr/bin/otool"
      "RANLIB=/usr/bin/ranlib"
    ];

    # Use the arch command to explicitly specify architecture, so that
    # configure and its subprocesses would pick up the architecture we
    # choose via the system argument.
    preConfigure = pkgs.lib.optionalString (system == "aarch64-darwin") ''
      substituteInPlace configure \
        --replace-fail "#! /bin/sh" "#!/usr/bin/env -S /usr/bin/arch -arm64 /bin/sh"
    '' + pkgs.lib.optionalString (system == "x86_64-darwin") ''
      substituteInPlace configure \
        --replace-fail "#! /bin/sh" "#!/usr/bin/env -S /usr/bin/arch -x86_64 /bin/sh"
    '' + ''
      unset DEVELOPER_DIR SDKROOT
      export DEVELOPER_DIR="$(/usr/bin/xcode-select --print-path)"
      export SDKROOT="$(/usr/bin/xcrun --sdk macosx --show-sdk-path)"
    '';

    dontPatchShebangsInConfigure = true;

    # N.B. Work around #20253.
    nativeBuildInputs = [ pkgs.gnused ];

    dontBuild = true;

    enableParallelInstalling = true;

    dontFixup = true;

    # Sanity check: verify that we can compile hello world.
    doInstallCheck = true;
    installCheckPhase = ''
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
        scheme-small collection-xetex fncychap tex-gyre titlesec tabulary varwidth
        framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
    };
  fonts = with pkgs; makeFontsConf { fontDirectories = [ dejavu_fonts ]; };

  llvm = pkgs.llvm_21;
  llvm_clang = pkgs.llvmPackages_21.clang-unwrapped;
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
