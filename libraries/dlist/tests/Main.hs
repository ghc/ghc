{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif

--------------------------------------------------------------------------------

-- | Test runner.
module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
import qualified DNonEmptyProperties
#endif
import qualified OverloadedStrings

--------------------------------------------------------------------------------

main :: IO ()
main = do
  DListProperties.test
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
  DNonEmptyProperties.test
#endif
  OverloadedStrings.test
