#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

. ./travis-common.sh

# ---------------------------------------------------------------------
# Bootstrap cabal, to verify bootstrap.sh script works.
# ---------------------------------------------------------------------

bootstrap_jobs="-j"

(cd cabal-install && timed env EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh $bootstrap_jobs --no-doc)
timed $HOME/.cabal/bin/cabal --version
PATH=$HOME/.cabal/bin:$PATH

# ---------------------------------------------------------------------
# Verify that installation from tarball works.
# ---------------------------------------------------------------------

# The following scriptlet checks that the resulting source distribution can be
# built & installed.
install_from_tarball() {
   SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   export SRC_TGZ
   if [ -f "dist/$SRC_TGZ" ]; then
      cabal install --force-reinstalls $jobs "dist/$SRC_TGZ" -v2;
   else
      echo "expected 'dist/$SRC_TGZ' not found";
      exit 1;
   fi
}

timed cabal update

# NB: The cabal cleans here hack around an sdist bug where
# the bootstrapped Cabal/cabal-install may be built
# without a Paths_* module available.  Under some situations
# which I have not been able to reproduce except on Travis,
# cabal sdist will incorrectly pick up the left over dist
# directory from the bootstrap and then try to package
# up the Paths module, but to no avail because it is not
# available.  I ran out of patience trying to debug this
# issue, and it is easy enough to work around: clean first.

echo Cabal
(cd Cabal && timed cabal clean) || exit $?
(cd Cabal && timed cabal sdist) || exit $?
(cd Cabal && timed install_from_tarball) || exit $?

echo cabal-install
(cd cabal-install && timed cabal clean) || exit $?
(cd cabal-install && timed cabal sdist) || exit $?
(cd cabal-install && timed install_from_tarball) || exit $?
