#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

set -x

. ./travis-common.sh

# Read out ACCOUNT and REPO from the slug
# Cribbed from http://unix.stackexchange.com/a/53323/118117
ACCOUNT=${TRAVIS_REPO_SLUG%%"/"*}
REPO=${TRAVIS_REPO_SLUG#*"/"}

# TAG will be used to uniquely identify a matrix entry; we
# need to push each matrix entry to a separate branch.
TAG="$TRAVIS_OS_NAME-$GHCVER$TAGSUFFIX"

# This is the commit for which we want a GitHub status update
# ping to go to.  Note that it is NOT TRAVIS_COMMIT unconditionally,
# since if we have a pull request, this commit will be a merge
# commit which no one from GitHub will be able to see.
COMMIT=${TRAVIS_PULL_REQUEST_SHA:-$TRAVIS_COMMIT}

# This is just to help you correlate the build to what it's for
if [ "x$TRAVIS_PULL_REQUEST" != "xfalse" ]; then
    ORIGIN="${TRAVIS_REPO_SLUG}/pull/$TRAVIS_PULL_REQUEST"
    URL="pull/${TRAVIS_PULL_REQUEST}"
else
    ORIGIN="${TRAVIS_REPO_SLUG}/${TRAVIS_BRANCH}"
    URL="commits/${TRAVIS_BRANCH}"
fi

# Git will complain if these fields don't work when committing,
# so set them up.
git config --global user.name "$(git --no-pager show -s --format='%an' $COMMIT)"
git config --global user.email "$(git --no-pager show -s --format='%ae' $COMMIT)"
git config --global push.default simple

cd travis

# Setup SSH key we will use to push to binaries repository.
# umask to get the permissions to be 600 (not 400, because the deploy
# script in .travis.yml is going to clobber this private key)
(umask 177 && cp id_rsa $HOME/.ssh/id_rsa)

# Setup SSH keys
ssh-keyscan github.com >> $HOME/.ssh/known_hosts

cd binaries

# Setup binaries repository for pushing
git init
# TODO: Update this
git remote add origin git@github.com:haskell-pushbot/cabal-binaries.git

# Make some final modifications to .travis.yml based so
# that downstream builds with the correct configuration
echo "env: GHCVER=$GHCVER UPSTREAM_BUILD_DIR=$TRAVIS_BUILD_DIR CABAL_LIB_ONLY=$CABAL_LIB_ONLY TEST_OTHER_VERSIONS=$TEST_OTHER_VERSIONS" >> .travis.yml
echo "os: $TRAVIS_OS_NAME" >> .travis.yml
if [ "x$GHCVER" = "x7.8.4" ] && [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    echo "osx_image: xcode6.4" >> .travis.yml
fi

# Make directory layout
mkdir Cabal
mkdir cabal-install
cp -R $TRAVIS_BUILD_DIR/Cabal/tests                                  Cabal
cp -R $TRAVIS_BUILD_DIR/cabal-install/tests                          cabal-install
# Copy in credentials so we can delete branch when done
cp $TRAVIS_BUILD_DIR/travis/id_rsa .
# Install all of the necessary files for testing
cp $TRAVIS_BUILD_DIR/travis-install.sh .
cp $TRAVIS_BUILD_DIR/travis-common.sh .
# The binaries to test (statically linked, of course!)
cp ${CABAL_BDIR}/c/unit-tests/build/unit-tests/unit-tests                         Cabal
cp ${CABAL_BDIR}/c/check-tests/build/check-tests/check-tests                 Cabal
cp ${CABAL_BDIR}/c/parser-tests/build/parser-tests/parser-tests                 Cabal
cp ${CABAL_BDIR}/c/parser-hackage-tests/build/parser-hackage-tests/parser-hackage-tests Cabal
if [ "x$CABAL_LIB_ONLY" != "xYES" ]; then
    cp ${CABAL_INSTALL_BDIR}/build/cabal/cabal                       cabal-install
fi

# Add, commit, push
git add .
# The JSON in the commit message is used by the webhook listening
# on the downstream repo to figure out who to communicate the
# status update back to
git commit -m '{"origin":"'$ORIGIN'",

"url":"'$URL'",
"account":"'$ACCOUNT'",
"repo":"'$REPO'",
"commit": "'$COMMIT'",
"tag":"'$TAG'"
}'
travis_retry git push -f origin "HEAD:$TAG/$COMMIT"
