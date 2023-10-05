{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module T16341 where

import Language.Haskell.TH.Syntax (Lift)

data Foo a where
  Foo1 :: Foo Int
  Foo2 :: (Bool -> Bool) -> Foo Bool

-- These instances should work whether or not `Foo2` is a constructor in
-- `Foo`, because the `Foo Int` designation precludes `Foo2` from being
-- a reachable constructor
deriving instance Show (Foo Int)
deriving instance Eq (Foo Int)
deriving instance Ord (Foo Int)
deriving instance Lift (Foo Int)

data Bar a b where
  Bar1 :: b -> Bar Int b
  Bar2 :: (Bool -> Bool) -> b -> Bar Bool b

deriving instance Functor (Bar Int)
deriving instance Foldable (Bar Int)
deriving instance Traversable (Bar Int)
