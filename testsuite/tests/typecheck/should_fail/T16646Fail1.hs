{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T16646Fail1 where

import GHC.Exts

f :: forall {rr :: RuntimeRep} dt st (r :: TYPE rr). (dt => r) -> st -> r
f = magicDict @dt
