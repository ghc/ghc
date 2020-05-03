{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module T18127b where

import GHC.Generics

data T1 = MkT1 (forall a. a) deriving (Eq, Generic)
data T2 a = MkT2 (Show a => a) deriving (Eq, Generic)
