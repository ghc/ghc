{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
module T20375 where

import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Exts (Int#, RuntimeRep(..), TYPE)
import GHC.Generics (Generic, Generic1)

-- Test special-casing for unlifted types (e.g., Int#)
data D1 (a :: TYPE IntRep) b = MkD1 a b
deriving stock instance Eq (D1 Int# Int)
deriving stock instance Ord (D1 Int# Int)
deriving stock instance Generic (D1 Int# Int)
deriving stock instance Generic1 (D1 Int#)
deriving stock instance Show (D1 Int# Int)

-- Test special-casing for tuples
data D2 p a = MkD2 (p a Int)
deriving stock instance Foldable (D2 (,))
deriving stock instance Functor (D2 (,))
deriving stock instance Traversable (D2 (,))

-- Ensure that validity checks don't get tripped up by a runtime-polymorphic
-- type that is instantiated to something runtime-monomorphic.
newtype D3 (a :: TYPE r) = MkD3 a
deriving stock instance Bounded (D3 Int)
deriving stock instance Data (D3 Int)
deriving stock instance Eq (D3 Int)
deriving stock instance Foldable D3
deriving stock instance Functor D3
deriving stock instance Generic (D3 Int)
deriving stock instance Generic1 D3
deriving stock instance Ix (D3 Int)
deriving stock instance Ord (D3 Int)
deriving stock instance Read (D3 Int)
deriving stock instance Show (D3 Int)
deriving stock instance Traversable D3
