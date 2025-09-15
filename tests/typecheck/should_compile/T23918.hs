module T23918 where

import Data.Kind

f :: forall (a :: Type). a -> a
f = g @a

g :: forall (k :: Type) (a :: Type) (r :: k). a -> a
g = g
