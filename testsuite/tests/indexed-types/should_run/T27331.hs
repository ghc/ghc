
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Main where

import GHC.Exts
import GHC.Internal.Box ( Box )
import GHC.Internal.Prim ( box, unbox )


foo1 :: Int# -> Box Int#
foo1 = box

bar1 :: Box Int# -> Int#
bar1 = unbox

foo2 :: (# Int#, Float# #) -> Box (# Int#, Float# #)
foo2 = box

bar2 :: Box (# Int#, Float# #) -> (# Int#, Float# #)
bar2 = unbox

foo3 :: (# Int# | Float# #) -> Box (# Int# | Float# #)
foo3 = box

bar3 :: Box (# Int# | Float# #) -> (# Int# | Float# #)
bar3 = unbox

-- Test boxing at TupleRep works for types that aren't unboxed tuples on the nose

-- Case 1: unreduced type family application
type F :: TYPE r -> TYPE (TupleRep '[ r, r, r ])
type family F a = r | r -> a where
  F Int# = (# Int#, Int#, Int# #)

foo4 :: forall (a :: TYPE IntRep). F a -> Box ( F a )
foo4 = box

bar4 :: forall (a :: TYPE IntRep). Box ( F a ) -> F a
bar4 = unbox

-- Case 2: unlifted newtype
type N :: TYPE (SumRep '[ TupleRep '[], r ]) -> TYPE (SumRep '[ TupleRep '[], r ])
newtype N a = MkN a

foo5 :: forall (a :: TYPE (SumRep '[ TupleRep '[], IntRep ])). N a -> Box ( N a )
foo5 = box

bar5 :: forall (a :: TYPE (SumRep '[ TupleRep '[], IntRep ])). Box ( N a ) -> N a
bar5 = unbox

main :: IO ()
main = do
  case bar1 $ foo1 1# of { j -> print $ I# j }
  case bar2 $ foo2 (# 2#, 2.0# #) of { (# j, f #) -> do { print $ I# j; print $ F# f } }
  case bar3 $ foo3 (# 3# | #) of { (# j | #) -> print $ I# j; (# | f #) -> print $ F# f }
  case bar4 @Int# $ foo4 @Int# (# 4#, 5#, 6# #) of { (# i, j, k #) -> print [ I# i, I# j, I# k ] }
  case bar5 $ foo5 $ MkN (# | 7# #) of { MkN n -> case n of { (# j | #) -> putStrLn "(# (##)|#)" ; (# | j #) -> print $ I# j } }
