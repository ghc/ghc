{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module T25621Fail where

import GHC.Generics

class Cls a where
  m :: a
  default m :: (Generic a, GCls (Rep a)) => a
  m = to gm

class GCls f where
  gm :: f a

instance GCls f => GCls (M1 _i _c f) where
  gm = M1 gm

instance (c ~ ()) => GCls (K1 _i c) where
  gm = K1 ()

data Foo a = MkFoo a
  deriving stock Generic
  deriving anyclass Cls
  -- The derived `Cls` instance doesn't typecheck, as `approximateWC` won't
  -- quantify over an equality constraint unless one side of the equality is
  -- headed by a type family application. The inferred equality constraint in
  -- this instance (a ~ ()) doesn't meet this criterion.
