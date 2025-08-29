{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Data.Kind (Constraint, Type)

type Sing :: k -> Type
type family Sing @k

type Tuple0 :: Type
data Tuple0 = MkTuple0

type STuple0 :: Tuple0 -> Type
data STuple0 z where
  SMkTuple0 :: STuple0 MkTuple0
type instance Sing @Tuple0 = STuple0

type U1 :: Type
data U1 = MkU1

type SU1 :: U1 -> Type
data SU1 z where
  SMkU1 :: SU1 MkU1
type instance Sing @U1 = SU1

type Generic :: Type -> Constraint
class Generic a where
  type Rep a :: Type

type PGeneric :: Type -> Constraint
class PGeneric a where
  type PFrom (x :: a)     :: Rep a

type SGeneric :: Type -> Constraint
class SGeneric a where
  sFrom :: forall (x :: a). Sing x -> Sing (PFrom x)

instance SGeneric Tuple0 where
  sFrom SMkTuple0 = SMkU1

instance Generic Tuple0 where
  type Rep Tuple0 = U1

instance PGeneric Tuple0 where
  type PFrom MkTuple0  = MkU1

type SDecide :: Type -> Constraint
class SDecide a where
  sDecide :: forall (x :: a) (y :: a). Sing x -> Sing y -> Bool

instance SDecide U1 where
  sDecide SMkU1 SMkU1 = True

sDecideDefault :: Sing MkTuple0 -> Sing MkTuple0 -> Bool
sDecideDefault s1 s2 = sDecide (sFrom s1) (sFrom s2)
