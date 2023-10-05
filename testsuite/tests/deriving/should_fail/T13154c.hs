{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
module T13154c where

import GHC.Exts

-- Test some nonsense configurations

class Foo1 (a :: TYPE ('TupleRep '[]))
deriving stock   instance Foo1 a
deriving stock   instance Foo1 (##)
deriving newtype instance Foo1 a
deriving newtype instance Foo1 (##)

class Foo2
deriving stock   instance Foo2
deriving newtype instance Foo2
