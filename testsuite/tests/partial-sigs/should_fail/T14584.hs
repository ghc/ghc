{-# OPTIONS_GHC -fdefer-type-errors #-}     -- Very important to this bug!
{-# Language PartialTypeSignatures #-}
{-# Language TypeFamilyDependencies, KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}
{-# Language NoImplicitPrelude #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleInstances #-}
{-# Language TypeOperators #-}
{-# Language ScopedTypeVariables #-}
{-# Language DefaultSignatures #-}
{-# Language FunctionalDependencies #-}
{-# Language UndecidableSuperClasses #-}
{-# Language UndecidableInstances #-}
{-# Language TypeInType #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language InstanceSigs, TypeApplications #-}

module T14584 where

import Data.Monoid
import Data.Kind

data family Sing (a::k)

class SingKind k where
  type Demote k = (res :: Type) | res -> k
  fromSing :: Sing (a::k) -> Demote k

class SingI (a::k) where
  sing :: Sing a

data ACT  :: Type -> Type -> Type
data MHOM :: Type -> Type -> Type

type m %%-  a  = ACT  m a  -> Type
type m %%-> m' = MHOM m m' -> Type

class Monoid m => Action (act :: m %%- a) where
  act :: m -> (a -> a)

class (Monoid m, Monoid m') => MonHom (mhom :: m %%-> m') where
  monHom :: m -> m'

data MonHom_Distributive m :: (m %%- a) -> (a %%-> a)

type Good k = (Demote k ~ k, SingKind k)

instance (Action act, Monoid a, Good m) => MonHom (MonHom_Distributive m act :: a %%-> a) where
  monHom :: a -> a
  monHom = act @_ @_ @act (fromSing @m (sing @m @a :: Sing _))
