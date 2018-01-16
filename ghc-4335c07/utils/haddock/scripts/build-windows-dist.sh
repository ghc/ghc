# mini script for building the relocatable Windows binary distribution.
#
#    sh build-windows-dist.sh
#
# NB. the Cabal that shipped with GHC 6.6 isn't enough for this, because it
# is missing this patch:
# 
# Fri Oct 13 11:09:41 BST 2006  Simon Marlow <simonmar@microsoft.com>
#   * Fix getDataDir etc. when bindir=$prefix
#
# So you need to use a more recent Cabal.  GHC 6.6 is fine for building the
# package, though.

ghc --make Setup
./Setup configure --prefix=`pwd`/install --bindir='$prefix' --libdir='$prefix' --datadir='$prefix'
./Setup build
./Setup install
echo Now zip up `pwd`/install as "haddock-<version>-Win32.zip"
