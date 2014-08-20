{-# LANGUAGE RankNTypes, MagicHash, TypeFamilies, PolyKinds #-}

module T9357 where
import GHC.Exts

type family F (a :: k1) :: k2
type instance F Int# = Int
type instance F (forall a. a->a) = Int
