module M where

import GHC.Classes
default ()

data Foo a = Nothing | Just a
  deriving (Eq, Ord)
