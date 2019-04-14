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
if [[ -z ${INTEGER_LIBRARY:-} ]]; then INTEGER_LIBRARY=integer-gmp; fi
if [[ -z ${BUILD_FLAVOUR:-} ]]; then BUILD_FLAVOUR=perf; fi

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_SPHINX_HTML=$BUILD_SPHINX_HTML
BUILD_SPHINX_PDF=$BUILD_SPHINX_PDF
BeConservative=YES
INTEGER_LIBRARY=$INTEGER_LIBRARY
EOF

cat <<EOF >> mk/build.mk
BuildFlavour=$BUILD_FLAVOUR
ifneq "\$(BuildFlavour)" ""
include mk/flavours/\$(BuildFlavour).mk
endif
EOF

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        echo 'HADDOCK_DOCS = NO' >> mk/build.mk
        echo 'WERROR=' >> mk/build.mk
        # https://circleci.com/docs/2.0/env-vars/#interpolating-environment-variables-to-set-other-environment-variables
        echo 'export PATH=/opt/ghc/bin:$PATH' >> $BASH_ENV
      else
        fail "TARGET=$target not supported"
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
    echo "libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-intree-gmp" >> mk/build.mk
    ;;
  *)
    fail "uname=$(uname) not supported"
esac

echo "================================================="
echo "Build.mk:"
echo ""
cat mk/build.mk
echo "================================================="
