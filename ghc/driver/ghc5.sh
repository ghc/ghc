# generic driver for ghc-5.X
exec $GHC_TOPDIR/bin/$GHC_PLATFORM/ghc-4.11 -B$GHC_TOPDIR/lib/$GHC_PLATFORM "$@"
