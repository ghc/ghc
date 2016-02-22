{-# LANGUAGE RankNTypes, MagicHash #-}

module T7888 where
import GHC.Err( undefined )
import GHC.Prim

{- The fix for #11431 makes this no longer work. But it shouldn't really,
without impredicativity.
f :: (forall a. a) -> b
f = undefined
-}

-- this still had better work, though!
g :: Int -> Int#
g _ = undefined
