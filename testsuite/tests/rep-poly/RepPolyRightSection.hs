{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyRightSection where

import GHC.Exts

g :: forall r (a :: TYPE r) b. a -> b -> a
g = undefined

test2 :: forall r (a :: TYPE r). a -> a
test2 = ( `g` undefined )
