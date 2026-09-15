{-# LANGUAGE OverloadedLabels, TransformListComp, MonadComprehensions, MagicHash #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples, UnboxedSums #-}

module Main where
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Exts


{- This is a variant on the accepted program:

blah :: [Bool] -> [(Int, Bool)]
blah bs = [ (I# i, b) | b <- bs, let i = 1#, then reverse ]

That program has 'i :: Int# :: TYPE IntRep'.

The current program is a variant in which we only learn the
representation of 'i' after constraint solving.
-}

data A = A Int deriving Show
instance IsLabel "x" (A -> Int#) where
  fromLabel (A (I# i)) = i

blah1 :: [A] -> [(Int, A)]
blah1 as = [ (I# i, a) | a <- as, let i = #x a, then reverse ]


-- ditto, with a more complicated unboxed type

data B = B Int Float deriving Show
instance IsLabel "x" (B -> (# Int#, (# (# #) | Float# #) #)) where
  fromLabel (B (I# i) (F# f)) = (# i, (# | f #) #)

blah2 :: [B] -> [((Int, Maybe Float), B)]
blah2 as = [ (wrap p, a) | a <- as, let p = #x a, then reverse ]

wrap :: (# Int#, (# (# #) | Float# #) #) -> ( Int, Maybe Float )
wrap (# i, j #) =
  ( \ m -> ( I# i, m ) ) $
    case j of
      (# (# #) | #) -> Nothing
      (# | f #) -> Just $ F# f


main :: IO ()
main = do
  print $ blah1 [A 1, A 2, A 3]
  print $ blah2 [B 11 11.1, B 22 22.2, B 33 33.3]
