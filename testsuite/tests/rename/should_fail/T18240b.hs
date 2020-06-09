{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module T18240b where

import Data.Proxy

data T a b

class W x y z
instance W x y (T a b)

newtype Foo a b = MkFoo (T a b)
deriving via (forall x. T x y)           instance W x y (Foo a b)
deriving via forall x. forall y. T x y   instance W x y (Foo a b)
deriving via forall x. (forall y. T x y) instance W x y (Foo a b)

class C1 x
class C2 x y z

data Bar = MkBar
  deriving anyclass ( C1
                    , (forall x. C2 x y)
                    , forall x. forall y. C2 x y
                    , forall x. (forall y. C2 x y)
                    )
