#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

echo 'BUILD_SPHINX_HTML = NO' > mk/validate.mk
echo 'BUILD_SPHINX_PDF = NO' >> mk/validate.mk
hackage_index_state="@1522046735"

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BeConservative=YES
EOF

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        add-apt-repository -y ppa:hvr/ghc
        apt-get update -qq
        apt-get install -qy ghc-8.0.2 cabal-install-1.24 alex happy \
                            ncurses-dev git make automake autoconf gcc perl \
                            python3 texinfo xz-utils lbzip2 patch
        cabal update
        cabal install --reinstall hscolour --index-state=$hackage_index_state
        ln -s $HOME/.cabal/bin/HsColour /usr/local/bin/HsColour

        echo 'HADDOCK_DOCS = NO' >> mk/build.mk
        echo 'WERROR=' >> mk/build.mk
        # https://circleci.com/docs/2.0/env-vars/#interpolating-environment-variables-to-set-other-environment-variables
        echo 'export PATH=/opt/ghc/bin:$PATH' >> $BASH_ENV
      else
        fail "TARGET=$target not supported"
      fi
    else
      cabal update
      cabal install --reinstall hscolour
      sudo ln -s /home/ghc/.cabal/bin/HsColour /usr/local/bin/HsColour || true
    fi
    ;;
  Darwin)
    if [[ -n ${TARGET:-} ]]; then
      fail "uname=$(uname) not supported for cross-compilation"
    fi
    # It looks like we already have python2 here and just installing python3
    # does not work.
    brew upgrade python
    brew install ghc cabal-install ncurses gmp
    cabal update
    cabal install --reinstall alex happy haddock hscolour --index-state=$hackage_index_state
    # put them on the $PATH, don't fail if already installed
    ln -s $HOME/.cabal/bin/alex /usr/local/bin/alex || true
    ln -s $HOME/.cabal/bin/happy /usr/local/bin/happy || true
    ln -s $HOME/.cabal/bin/HsColour /usr/local/bin/HsColour || true
    ;;
  *)
    fail "uname=$(uname) not supported"
esac
