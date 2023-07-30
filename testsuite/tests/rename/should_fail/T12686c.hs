module T12686 where

import Data.Proxy

class C a where
  x :: a -> Bool

data Bad = MkBad 'x
-- The 'x should be rejected in a civilised way