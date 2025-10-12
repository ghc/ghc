{-# LANGUAGE CPP #-}
module Example11 where

#if MIN_VERSION_ghc_xxxxx(1,2,3)
x = 1
#else
x = 2
#endif
