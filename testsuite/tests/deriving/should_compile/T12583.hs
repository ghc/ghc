{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module T12583 where

import Data.Ix

data Foo a where
  MkFoo :: (Eq a, Ord a, Ix a) => Foo a
deriving instance Eq  (Foo a)
deriving instance Ord (Foo a)
deriving instance Ix  (Foo a)
