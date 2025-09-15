{-# LANGUAGE MagicHash #-}
module M where

import GHC.Exts

i :: forall k (f :: k -> RuntimeRep) (g :: k) (a :: TYPE (f g)). a -> a
i = i

x = i 0#
