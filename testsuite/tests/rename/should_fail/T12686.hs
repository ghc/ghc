module T12686 where

import Data.Proxy

x = True

data Bad = Bad 'x
-- The 'x should be rejected in a civilised way

data AlsoBad = AlsoBad {
  a :: Int,
  b :: Either Int 'a }
-- Ditto 'a here
