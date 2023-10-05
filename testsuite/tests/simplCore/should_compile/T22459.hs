{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -O #-}

module Lib (foo) where

import qualified Data.Map as M

newtype Fix f = Fix (f (Fix f))

instance Eq (f (Fix f)) => Eq (Fix f) where
  Fix a == Fix b = a == b

instance Ord (f (Fix f)) => Ord (Fix f) where
  Fix a `compare` Fix b = a `compare` b

data Foo i r = Foo i r
  deriving (Eq, Ord)

newtype Bar a = Bar (M.Map Char (M.Map (Fix (Foo ())) Word))

foo :: Bar a -> Bar a -> Bar a
foo (Bar a) (Bar b) = Bar (M.unionWith M.union a b)
