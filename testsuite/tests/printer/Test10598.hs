{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test10598 where

class C a where
  c :: proxy a -> Int
  c _ = 42

instance C Int where
  c _ = 27

newtype Foo = MkFoo Int
  deriving          Eq
  deriving stock    Ord
  deriving anyclass C
deriving newtype instance Show Foo
