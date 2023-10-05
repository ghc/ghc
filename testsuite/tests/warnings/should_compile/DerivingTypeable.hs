module DerivingTypeable where

import Data.Typeable

data Foo =
    Foo Int
  | Bar Char
  deriving Typeable
