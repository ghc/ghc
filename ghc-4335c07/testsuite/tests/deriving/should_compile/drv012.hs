{-# LANGUAGE GADTs #-}

-- !!! deriving for GADTs which declare Haskell98 data types.
-- bug reported as http://ghc.haskell.org/trac/ghc/ticket/902
module ShouldSucceed where

data Maybe1 a where {
      Nothing1 :: Maybe1 a ;
      Just1    :: a -> Maybe1 a
    } deriving (Eq,Ord)
