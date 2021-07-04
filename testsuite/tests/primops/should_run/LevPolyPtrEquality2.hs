{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import GHC.Exts
import GHC.Types

data PEither a b :: UnliftedType where
  PLeft  :: a -> PEither a b
  PRight :: b -> PEither a b

main :: IO ()
main = do
  let
    a, b, c :: PEither Bool Int
    a = PRight 1
    b = case a of { PLeft a -> PLeft (not a) ; r -> r }
    c = PLeft False
    d :: Either Bool Int
    d = Right 1
  putStr "eq a b: "
  print $ isTrue# ( reallyUnsafePtrEquality# a b )
  putStr "eq a c: "
  print $ isTrue# ( reallyUnsafePtrEquality# a c )
  putStr "eq a d: "
  print $ isTrue# ( reallyUnsafePtrEquality# a d )
