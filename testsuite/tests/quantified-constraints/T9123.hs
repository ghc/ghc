{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs, RoleAnnotations  #-}

module T9123 where

import Data.Coerce

type role Wrap representational nominal
newtype Wrap m a = Wrap (m a)

class Monad' m where
  join' :: forall a. m (m a) -> m a

-- Tests the superclass stuff
instance (forall p q. Coercible p q => Coercible (m p) (m q), Monad' m) => Monad' (Wrap m) where
  join' :: forall a. Wrap m (Wrap m a) -> Wrap m a
  join' = coerce @(m (m a) -> m a)
                 @(Wrap m (Wrap m a) -> Wrap m a)
                 join'

