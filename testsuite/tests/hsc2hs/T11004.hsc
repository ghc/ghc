{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module T11004 where

import Data.Kind (Type)

data Foo = Foo' | Bar

data Indexed :: Foo -> Type where
  IndexedA :: Indexed ' Foo'
  IndexedB :: Indexed 'Bar
