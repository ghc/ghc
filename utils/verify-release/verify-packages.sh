#!/bin/bash
#
# This script will verify that the packages in ghc,
# correspond to packages on hackage.
rm -fR ghc-packages hackage-packages package-diffs
mkdir -p ghc-packages hackage-packages package-diffs
# We'll skip Cabal and tarballs, while looking only at packages in libraries
# that we reference as gitmodules.
for lib in $(git submodule status|grep libraries|grep -v tarballs|grep -v Cabal|awk -F\  '{ print $2 }'); do
    (cd $lib && cabal new-sdist -o ../../ghc-packages);
done

for pkg in $(cd ghc-packages && ls *.tar.gz); do
    PKG=${pkg%%.tar.gz}
    (cd hackage-packages && cabal get --pristine ${PKG})
    (cd hackage-packages/${PKG} && git init && git add . && git commit -q -m "hackage packages" && rm -fR *)
    (cd hackage-packages && tar xzf ../ghc-packages/$pkg)
    (cd hackage-packages/${PKG} && git -c core.fileMode=false diff > ../$PKG.patch)
done
find hackage-packages -name "*.patch" -not -empty -type f -print -exec false {} +
