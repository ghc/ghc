module T12067a (MT(..), M) where

import Data.Functor.Identity

newtype MT m b = MT (m b)
type M b = MT Identity b
