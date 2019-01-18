#!/bin/bash
#
# This script will verify that the packages in ghc,
# correspond to packages on hackage.
rm -fR ghc-packages hackage-packages package-diffs
mkdir -p ghc-packages hackage-packages package-diffs
# We'll skip Cabal and tarballs, while looking only at packages in libraries
# the we reference as gitmodules.
for lib in $(git submodule status|grep libraries|grep -v tarballs|grep -v Cabal|awk -F\  '{ print $2 }'); do
    (cd $lib && cabal new-sdist -o ../../ghc-packages);
done

for pkg in $(cd ghc-packages && ls *.tar.gz); do
    PKG=${pkg%%.tar.gz}
    (cd hackage-packages && cabal get --pristine ${PKG})
    (cd ghc-packages && tar xzf $pkg)
    diff -ur hackage-packages/${PKG} ghc-packages/${PKG} \
	| sed "s/hackage-packages\/${PKG}/hackage-package-${PKG}/"g \
	| sed "s/ghc-packages\/${PKG}/ghc-package-${PKG}/"g \
	      > package-diffs/${PKG}.patch
done
ls -lah package-diffs
