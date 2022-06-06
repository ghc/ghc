{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyTuple2 where

import GHC.Exts

type RR :: RuntimeRep
type family RR where { RR = FloatRep }
type F :: TYPE RR
type family F where { F = Float# }

{-# NOINLINE expensive #-}
expensive :: Float -> Float
expensive x = cos x ** 123.45

tup x = (# , #) @LiftedRep @RR (expensive x)
