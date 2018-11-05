#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

hackage_index_state="@1522046735"

if [[ -z ${BUILD_SPHINX_HTML:-} ]]; then BUILD_SPHINX_HTML=YES; fi
if [[ -z ${BUILD_SPHINX_PDF:-} ]]; then BUILD_SPHINX_PDF=YES; fi

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_SPHINX_HTML=$BUILD_SPHINX_HTML
BUILD_SPHINX_PDF=$BUILD_SPHINX_PDF
BeConservative=YES
EOF

if [[ -z ${ENABLE_DWARF:-} ]]; then
  echo "GhcLibHcOpts=-g3" >> mk/build.mk
fi

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
      if [[ -z ${ENABLE_DWARF:-} ]]; then
        apt-get install -qy libdw1-dev
      fi
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

    pip3 install sphinx
    # PDF documentation disabled as MacTeX apparently doesn't include xelatex.
    #brew cask install mactex

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

echo "================================================="
echo "Build.mk:"
echo ""
cat mk/build.mk
echo "================================================="
