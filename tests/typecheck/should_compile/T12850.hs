{-# LANGUAGE ExplicitForAll, MagicHash, KindSignatures #-}
module T12850 where

import GHC.Types (RuntimeRep(..), TYPE)

f :: forall (x :: TYPE 'IntRep). x -> x
f x = x

g = ()
    where h = f 0#
