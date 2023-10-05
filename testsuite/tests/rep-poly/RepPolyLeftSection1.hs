{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyLeftSection1 where

import GHC.Exts

f :: forall a r (b :: TYPE r). a -> b -> b
f = undefined

test1 :: forall r (b :: TYPE r). b -> b
test1 = ( undefined `f` )

