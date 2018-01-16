{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module T12594 where

import GHC.Generics

data Action = Action

class ToField a where
  toField :: a -> Action

instance ToField Int where
  -- Not the actual instance, but good enough for testing purposes
  toField _ = Action

class ToRow a where
    toRow :: a -> [Action]
    default toRow :: (Generic a, GToRow (Rep a)) => a -> [Action]
    toRow = gtoRow . from

class GToRow f where
    gtoRow :: f p -> [Action]

instance GToRow f => GToRow (M1 c i f) where
    gtoRow (M1 x) = gtoRow x

instance (GToRow f, GToRow g) => GToRow (f :*: g) where
    gtoRow (f :*: g) = gtoRow f ++ gtoRow g

instance (ToField a) => GToRow (K1 R a) where
    gtoRow (K1 a) = [toField a]

instance GToRow U1 where
    gtoRow _ = []

data Foo = Foo { bar :: Int }
  deriving (Generic, ToRow)
