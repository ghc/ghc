#!/usr/bin/env bash

# This script supports testing the compatibility of the in-tree `base` with
# different non-in-tree GHCs. We deliberately use a script separate from
# `.gitlab/ci.sh` for this purspose, because such support is supposed to become
# part of the CI infrastructure of a separately maintained `base`.
#
# Currently, this script can only test that the in-tree `base` can be *built*
# with certain *released* GHCs.

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

# Create directories for other GHCs
mkdir other-ghcs
mkdir other-ghcs/src
mkdir other-ghcs/opt

# Save project root
project_root=$PWD

# Create Cabal file for `base`
cd libraries/base
sed -E -e 's/^( *ghc-internal)[^[:alnum:]-].*(,|$)/\1\2/' \
  < base.cabal.in \
  > base.cabal
cd ${project_root}

# Build `base` with the different GHCs
for ghc_version in ${ghc_versions}
do
  # Install the GHC
  cd other-ghcs/src
  archive_file=ghc-${ghc_version}-${platform}.tar.xz
  curl https://downloads.haskell.org/~ghc/${ghc_version}/${archive_file} \
    >${ghc_version}.tar.xz
  tar -xJf ${ghc_version}.tar.xz
  cd ghc-${ghc_version}-*
  ./configure --prefix "${project_root}/opt/${ghc_version}"
  make install
  cd ${project_root}

  # Build `base` with the installed GHC
  cd libraries/base
  cabal build \
    --with-compiler "${project_root}/other-ghcs/opt/${ghc_version}/bin/ghc" \
    --allow-boot-library-installs \
    -O0
  cd ${project_root}
done
