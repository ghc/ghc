{-# LANGUAGE DataKinds #-}
module T15073 where

import T15073a

newtype Foo a = MkFoo a
  deriving P
