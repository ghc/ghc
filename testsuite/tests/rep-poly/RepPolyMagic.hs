{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyMagic where

import GHC.Exts
import GHC.Magic

foo :: forall a r (b :: TYPE r). a -> b -> b
foo = seq

bar :: forall r (a :: TYPE r). (a -> a) -> (a -> a)
bar = oneShot
