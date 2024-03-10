let sources = import ./nix/sources.nix; in

{ nixpkgs ? (import sources.nixpkgs {}) }:

with nixpkgs;
let
  fetch-gitlab-artifacts = nixpkgs.callPackage ./fetch-gitlab-artifacts {};
  mk-ghcup-metadata = nixpkgs.callPackage ./mk-ghcup-metadata { fetch-gitlab=fetch-gitlab-artifacts;};


  bindistPrepEnv = pkgs.buildFHSUserEnv {
    name = "enter-fhs";
    targetPkgs = pkgs: with pkgs; [
      # all
      gcc binutils gnumake gmp ncurses5 git elfutils
      # source-release.sh
      xorg.lndir curl python3 which automake autoconf m4 file
      haskell.compiler.ghc8107 haskellPackages.happy haskellPackages.alex
    ];
    runScript = "$SHELL -x";
  };

  scripts = stdenv.mkDerivation {
    name = "rel-eng-scripts";
    nativeBuildInputs = [ makeWrapper ];
    preferLocalBuild = true;
    buildCommand = ''
      mkdir -p $out/bin

      makeWrapper ${./recompress-all} $out/bin/recompress-all \
        --prefix PATH : ${gnumake}/bin \
        --prefix PATH : ${gnutar}/bin \
        --prefix PATH : ${lzip}/bin \
        --prefix PATH : ${bzip2}/bin \
        --prefix PATH : ${gzip}/bin \
        --prefix PATH : ${xz}/bin \
        --prefix PATH : ${zip}/bin

      makeWrapper ${./upload.sh} $out/bin/upload.sh \
        --prefix PATH : ${moreutils}/bin \
        --prefix PATH : ${lftp}/bin \
        --prefix PATH : ${lzip}/bin \
        --prefix PATH : ${zip}/bin \
        --prefix PATH : ${s3cmd}/bin \
        --prefix PATH : ${gnupg}/bin \
        --prefix PATH : ${pinentry}/bin \
        --prefix PATH : ${python3}/bin \
        --prefix PATH : $out/bin \
        --set ENTER_FHS_ENV ${bindistPrepEnv}/bin/enter-fhs \
        --set BASH ${bash}/bin/bash

      makeWrapper ${./upload_ghc_libs.py} $out/bin/upload-ghc-libs
    '';
  };

in
  symlinkJoin {
    name = "ghc-rel-eng";
    preferLocalBuild = true;
    paths = [
      scripts
      fetch-gitlab-artifacts
      mk-ghcup-metadata
    ];
  }
