{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeInType #-}
module T14579 where

import Data.Kind
import Data.Proxy

newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a)
  deriving Eq

newtype Glurp a = MkGlurp (Wat ('Proxy :: Proxy a))
  deriving Eq
