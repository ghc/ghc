{-# LANGUAGE ViewPatterns,DeriveDataTypeable #-}
module T4371 where

import Data.Typeable

data E1 = E1 deriving Typeable
data E2 = E2 deriving Typeable

f :: Typeable a => a-> ()
f x = case x of
  (cast -> Just E1) -> ()
  (cast -> Just E2) -> ()
