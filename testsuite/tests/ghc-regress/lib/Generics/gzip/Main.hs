{-# OPTIONS -fglasgow-exts #-}

{-

This test illustrates zipping.
We process two companies; see CompanyDatatypes.hs.
We insist on equality modulo one type-specific case:
whenever we encounter salaries we take the sum of the two.

-}

module Main where
import Data.Generics
import CompanyDatatypes

-- The main function which prints the result of zipping
main = print $ gzip (mkTT sumS) genCom genCom
  where

    -- Sum up two salaries
    sumS (S x) (S y) = S (x+y)

    -- Make a two-arguments, generic function transformer
    mkTT (f::a -> a -> a) x y =
      case (cast x,cast y) of
        (Just (x'::a),Just (y'::a)) -> cast (f x' y')
        _                           -> Nothing
