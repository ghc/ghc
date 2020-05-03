{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Bug where

import Data.Kind
import Data.Proxy

class    (forall (z :: k). Show (Proxy z)) => ShowProxy (k :: Type) where
instance (forall (z :: k). Show (Proxy z)) => ShowProxy (k :: Type)

data T :: Type -> Type

data U = A | B
