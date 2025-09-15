{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module T9725 where

import Data.Kind (Type)

data En = M Bool
class Kn (l :: En)

instance Kn (M b)

data Fac :: En -> Type where
  Mo :: Kn (M b) => Fac (M b)

data Fm :: Type -> Type where
  HiF :: Kn (ent b) => Fm (Fac (ent b)) -> Fm (O ent)
  MoF :: Kn (M b) => Fm (Fac (M b))

data O :: (k -> En) -> Type where
  Hi :: Fac (ent k) -> O ent

data Co :: (Type -> Type) -> Type -> Type where
  Ab :: (t -> f t) -> Co f t

-- Restricted kind signature:
--test :: forall (ent :: Bool -> En) . (forall i . Kn (ent i) => Fm (Fac (ent i))) -> Co Fm (O ent)

test :: forall ent . (forall i . Kn (ent i) => Fm (Fac (ent i))) -> Co Fm (O ent)
test de = Ab h
    where h :: O ent -> Fm (O ent)
          h (Hi m@Mo) = HiF (d m)
          d :: Kn (ent i) => Fac (ent i) -> Fm (Fac (ent i))
          d _ = de

{-
9725.hs:27:25:
    Could not deduce (Kn (ent k1)) arising from a use of ‘HiF’
    from the context (ent k1 ~ 'M b, Kn ('M b))
      bound by a pattern with constructor
                 Mo :: forall (b :: Bool). Kn ('M b) => Fac ('M b),
               in an equation for ‘h’
      at 9725.hs:27:19-20
    In the expression: HiF (d m)
    In an equation for ‘h’: h (Hi m@Mo) = HiF (d m)
    In an equation for ‘test’:
        test de
          = Ab h
          where
              h :: O ent -> Fm (O ent)
              h (Hi m@Mo) = HiF (d m)
              d :: Kn (ent i) => Fac (ent i) -> Fm (Fac (ent i))
              d _ = de
Failed, modules loaded: none.
-}
