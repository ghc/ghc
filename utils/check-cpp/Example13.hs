{-# LANGUAGE GHC_CPP #-}
-- {-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module Example13 where

foo =
#if MIN_VERSION_GLASGOW_HASKELL(19,13,20250101,0)
  'a'
#else
  'b'
#endif
