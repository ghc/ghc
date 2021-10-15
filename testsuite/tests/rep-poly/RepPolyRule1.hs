{-# LANGUAGE DataKinds, PolyKinds #-}

module RepPolyRule1 where

import GHC.Exts

f :: forall rep (a :: TYPE rep). a -> a
f = undefined
{-# NOINLINE f #-}

{-# RULES "f_id" forall (x :: (a :: TYPE rep)). f x = x #-}
