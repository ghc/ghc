{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bug where

import Data.Coerce
import Data.Kind
import Data.Proxy

newtype Wat (x :: Proxy (a :: Type)) = MkWat (Maybe a)
  deriving Eq

newtype Glurp a = MkGlurp (Wat ('Proxy :: Proxy a))

instance Eq a => Eq (Glurp a) where
  (==) = coerce @(Wat ('Proxy @a) -> Wat ('Proxy @a) -> Bool)
                @(Glurp a         -> Glurp a         -> Bool)
                (==)
