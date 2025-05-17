{-# Language    RankNTypes          #-}
{-# Language    TypeFamilies        #-}
{-# Language    ScopedTypeVariables #-}
{-# Language    TypeApplications    #-}
{-# Language    DataKinds           #-}
{-# Language    PolyKinds           #-}
{-# Language    TypeOperators       #-}
{-# Language    GADTs               #-}
{-# Language    FlexibleInstances   #-}

module T15797 where
import Data.Kind

class Ríki (obj :: Type) where
  type Obj :: obj -> Constraint
  type forall (k :: Type) . Obj @k = Bæ @k :: k -> Constraint

class    Bæ    (a :: k)
instance Bæ @k (a :: k)

data
  EQ :: forall ob. ob -> ob -> Type where
  EQ :: EQ a a

instance
  Ríki (EQ @ob)
