{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyTuple3 where

import GHC.Exts

type RR :: RuntimeRep
type family RR where { RR = FloatRep }

-- should be accepted
foo :: forall (a :: TYPE FloatRep). a -> a -> (# a, a #)
foo x = (# , #) @RR @RR x

-- should be rejected
bar :: forall (a :: TYPE IntRep). a -> a -> (# a, a #)
bar x = (# , #) @RR @RR x
