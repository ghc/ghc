{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- | Tests using the OverloadedStrings language extension.
module OverloadedStrings (test) where

--------------------------------------------------------------------------------

import qualified Data.DList as DList
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.DList.DNonEmpty as DNonEmpty
#endif

--------------------------------------------------------------------------------

test :: IO ()
test = do
  print $ "OverloadedStrings for DList:     " `DList.append` "success"
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
  print $ "OverloadedStrings for DNonEmpty: " `DNonEmpty.append` "success"
#endif
