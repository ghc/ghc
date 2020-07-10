# most recent commit on the nixos-20.03 branch from 10-07-2020.
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/6a00eba02a38cd0f71367adc42857395a36ab4cd.tar.gz") {} }:

pkgs.mkShell {
  shellHook = ''
    export PS1="\n\[\033[1;32m\][aarch64-ncg-dev-shell:\w]\$\[\033[0m\] "

    # this one will build a stock-ghc. We'll use that to build the llvm ghc.
    function build-ghc () {
      time (./boot && ./configure --prefix=$HOME/opt --silent && make -j -s)
    }
    #
    function build-llvm () {
      if [[ "$(basename $PWD)" == "ghc-llvm" ]]; then
        ./boot
        NM=aarch64-unknown-linux-gnu-nm \
        LD=aarch64-unknown-linux-gnu-ld.gold \
        AR=aarch64-unknown-linux-gnu-ar \
        AS=aarch64-unknown-linux-gnu-as \
        CC=aarch64-unknown-linux-gnu-cc \
        CXX=aarch64-unknown-linux-gnu-cxx \
        GHC=$HOME/opt/bin/ghc \
        ./configure --target=aarch64-unknown-linux-gnu --enable-bootstrap-with-devel-snapshot --silent
        cp ${./quick-cross.mk} mk/build.mk
        make -j -s
      else
        echo "WARN: not in ghc-llvm, not running!"
      fi
    }
    function build-ncg () {
      if [[ "$(basename $PWD)" == "ghc-ncg" ]]; then
        ./boot
        NM=aarch64-unknown-linux-gnu-nm \
        LD=aarch64-unknown-linux-gnu-ld.gold \
        AR=aarch64-unknown-linux-gnu-ar \
        AS=aarch64-unknown-linux-gnu-as \
        CC=aarch64-unknown-linux-gnu-cc \
        CXX=aarch64-unknown-linux-gnu-cxx \
        GHC=$HOME/opt/bin/ghc \
        ./configure --target=aarch64-unknown-linux-gnu --enable-bootstrap-with-devel-snapshot --silent
        cp ${./quick-cross-ncg.mk} mk/build.mk
        make -j -s
      else
        echo "WARN: not in ghc-ncg, not running!"
      fi
    }
    # call this with run-test ghc-ncg or ghc-llvm
    function run-test () {
      if [[ "$(basename $PWD)" == "ghc-test" ]]; then
        STAGE1_GHC=$PWD/../$1/inplace/bin/ghc-stage1 \
        WRAPPED_ISERV=${./iserv-wrapped} \
        SKIP_PERF_TESTS=YES \
        LD=aarch64-unknown-linux-gnu-ld \
        STRIP=aarch64-unknown-linux-gnu-strip \
        TEST_HC=$PWD/../$1/inplace/bin/ghc-stage1 \
        THREADS=64 \
        TEST_WRAPPER=qemu-aarch64 \
        stage=1 make test VERBOSE=1
      else
        echo "WARN: not in ghc-test, not running!"
      fi
    }
    echo "=== active worktrees ==="
    git worktree list
    echo "=== the following fuctions are available ==="
    echo "build-ghc -- build a stock ghc with prefix=\$HOME/opt"
    echo "  use this to build the stage2 ghc to use for the llvm and ncg builds"
    echo "build-llvm -- build the llvm ghc"
    echo "  e.g. time PATH=\$HOME/opt/bin:\$PATH build-llvm"
    echo "build-ghc -- build the ncg ghc"
    echo "  e.g. time PATH=\$HOME/opt/bin:\$PATH build-ncg"
    echo "run-test <arg> -- run the test-suite."
    echo "  run-test ghc-llvm, or run-test ghc-ncg"
  '';
  hardeningDisable = [ "format" "fortify" ];
  buildInputs = with pkgs; [
    # ghc build dependencies
    python3
    perl
    haskellPackages.alex
    haskellPackages.happy
    haskell.compiler.ghc883
    #(haskell.compiler.ghc883.override { ghcFlavour = "prof"; })
    cabal-install
    autoconf
    automake

    llvmPackages_9.llvm
    llvmPackages_9.clang

    # native libs
    gmp.dev
    zlib

    # cross toolchain
    pkgsCross.aarch64-multiplatform.buildPackages.binutils
    pkgsCross.aarch64-multiplatform.stdenv.cc

    # cross libs
    linuxHeaders
    elf-header
    pkgsCross.aarch64-multiplatform.gmp.dev

    # for testing
    qemu

    # tools
    git
    file
    htop
    less
    which
    ripgrep

  ];
}