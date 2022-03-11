{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module RepPolyCase1 where

import GHC.Exts

bar :: forall {r} (a :: TYPE r). () -> a
bar = error "no bar"

x :: forall {r} (a :: TYPE r) proxy. proxy a -> ()
x _ = case bar @a () of {}

