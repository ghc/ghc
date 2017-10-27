#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        add-apt-repository -y ppa:hvr/ghc
        apt-get update -qq
        apt-get install -qy ghc-8.0.2 cabal-install  alex happy ncurses-dev git openssh-client make automake autoconf gcc perl python3 texinfo xz-utils
        cabal update
        cabal install --reinstall hscolour
        ln -s $HOME/.cabal/bin/HsColour /usr/local/bin/HsColour
      else
        fail "TARGET=$target not supported"
      fi
    else
      # assuming Ubuntu
      apt-get update -qq
      apt-get install -qy git openssh-client make automake autoconf gcc perl python3 texinfo xz-utils
      cabal update
      cabal install --reinstall hscolour
    fi
    ;;
  Darwin)
    if [[ -n ${TARGET:-} ]]; then
      fail "uname=$(uname) not supported for cross-compilation"
    fi
    brew install ghc cabal-install python3 ncurses
    cabal update
    cabal install --reinstall alex happy haddock hscolour
    # put them on the $PATH, don't fail if already installed
    ln -s $HOME/.cabal/bin/alex /usr/local/bin/alex || true
    ln -s $HOME/.cabal/bin/happy /usr/local/bin/happy || true
    ln -s $HOME/.cabal/bin/hscolour /usr/local/bin/hscolour || true
    ;;
  *)
    fail "uname=$(uname) not supported"
esac
