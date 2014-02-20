{-# LANGUAGE DataKinds, PolyKinds, DeriveDataTypeable, StandaloneDeriving, TypeOperators #-}

module T5682 where

import Data.Typeable

data a :+: b = Mk a b
data Foo = Bool :+: Bool

type X = True ':+: False

deriving instance Typeable '(:+:)
deriving instance Typeable '(,,)
