{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module T20330 where

import GHC.Exts

ok1 :: forall (r :: RuntimeRep) (z :: TYPE r). z -> z
ok1 = raise# (5 :: Int)

ok2 = (raise# :: forall (r :: RuntimeRep) (z :: TYPE r). Int -> z -> z)
