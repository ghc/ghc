{-# LANGUAGE GHC_CPP #-}

module Example9 where

#ifdef __GLASGOW_HASKELL__
#endif

-- A comment
#ifdef __NHC__
#else
getTime = 2
#endif
