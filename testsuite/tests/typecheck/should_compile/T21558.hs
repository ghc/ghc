{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T21558 where

import Data.Map

type family IxValue (m :: *) :: *
type family Index (m :: *) :: *

data Op a where
  Insert :: (a ~ Map (Index a) (IxValue a)) => (Int, Index a, IxValue a) -> Op a

instance Eq (Op a) where

instance (Ord (Index a), Ord (IxValue a), Ord a) => Ord (Op a) where
  compare (Insert a1) (Insert b1) = compare a1 b1
