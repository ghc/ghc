#!/usr/bin/env bash

# This script supports testing the compatibility of the in-tree `base` with
# different non-in-tree GHCs. We deliberately use a script separate from
# `.gitlab/ci.sh` for this purspose, because such support is supposed to become
# part of the CI infrastructure of a separately maintained `base`.
#
# Currently, this script can only test that the in-tree `base` can be *built*
# with certain *released* GHCs.
#
# This script can run on both Unix and Windows. Since GHC’s infrastructure does
# not offer Cabal pre-installed on Windows, this script uses a `Setup.hs` file
# to build `base`, which works across operating systems.

# Establish error propagation
set -e -o pipefail

# Process arguments
if [ $# -lt 1 ]
then
  echo 'Build `base` with released GHCs'
  echo "Usage: base-ci ⟨platform⟩ ⟨ghc-version⟩ …"
  exit 1
fi >&2
platform=$1
shift
ghc_versions=$*

# Save project root
project_root=$PWD

# Create directories for other GHCs
mkdir other-ghcs
mkdir other-ghcs/src
mkdir other-ghcs/opt

# Amend the `base` sources
cd libraries/base
sed -E -e 's/^( *ghc-internal)[^[:alnum:]-].*(,|$)/\1\2/' \
  < base.cabal.in \
  > base.cabal
cat <<. >Setup.hs
import Distribution.Simple
main = defaultMain
.
cd ${project_root}

# Build `base` with the different GHCs
for ghc_version in ${ghc_versions}
do
  # Install the GHC
  ghc_installation=${project_root}/other-ghcs/opt/${ghc_version}
  cd other-ghcs/src
  archive_file=ghc-${ghc_version}-${platform}.tar.xz
  curl https://downloads.haskell.org/~ghc/${ghc_version}/${archive_file} \
    >${ghc_version}.tar.xz
  tar -xJf ${ghc_version}.tar.xz
  if [ -f ghc-${ghc_version}-*/configure ]
  then # Unix
    cd ghc-${ghc_version}-*
    ./configure --prefix "${ghc_installation}"
    make install
  else # Windows
    mv ghc-${ghc_version}-* "${ghc_installation}"
  fi
  cd ${project_root}

  # Build `base` with the installed GHC
  cd libraries/base
  "${ghc_installation}/bin/runghc" Setup.hs configure \
    --with-compiler "${ghc_installation}/bin/ghc" \
    -O0
  "${ghc_installation}/bin/runghc" Setup.hs build
  cd ${project_root}
done
