{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind
import Data.Proxy

class MyShow a where
  show :: a -> String

class    (forall (z :: k). MyShow (Proxy z)) => MyShowProxy (k :: Type) where
instance (forall (z :: k). MyShow (Proxy z)) => MyShowProxy (k :: Type)

data T :: Type -> Type

data U = A | B

instance forall (z :: U). MyShow (Proxy (z :: U)) where
  show _ = "U"

data U2 = A2 | B2

instance MyShow (Proxy A2) where
  show _ = "A2"

instance MyShow (Proxy B2) where
  show _ = "B2"

