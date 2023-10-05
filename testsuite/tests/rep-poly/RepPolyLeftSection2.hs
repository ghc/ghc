{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyLeftSection2 where

import GHC.Exts

f :: forall r (a :: TYPE r). a -> a -> a
f = undefined

test1 :: forall r (a :: TYPE r). a -> a
test1 = ( undefined `f` )

