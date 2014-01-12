{-# LANGUAGE RankNTypes, MagicHash #-}

module T7888 where
import GHC.Err( undefined )
import GHC.Prim

f :: (forall a. a) -> b
f = undefined

g :: Int -> Int#
g _ = undefined
