{-# LANGUAGE RankNTypes, MagicHash, TypeFamilies, PolyKinds #-}

module T9357 where
import GHC.Exts

type family F (a :: k1) :: k2

type instance F Int# = Int
 -- This one is actually OK (F is poly-kinded;
 -- c.f. #11120 comment:19

type instance F (forall a. a->a) = Int
 -- But this one is not (impredicative)
