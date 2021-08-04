{-# OPTIONS -fglasgow-exts #-}

module GZip (tests) where

{-

This test illustrates zipping for the company datatypes which we use a
lot. We process two companies that happen to agree on the overall
shape but differ in the salaries in a few positions. So whenever we
encounter salaries we take the maximum of the two.

-}

import Test.Tasty.HUnit

import Data.Generics
import CompanyDatatypes

-- The main function which prints the result of zipping
tests = gzip (\x y -> mkTT maxS x y) genCom1 genCom2 @=? output
  -- NB: the argument has to be eta-expanded to match
  --     the type of gzip's argument type, which is
  --     GenericQ (GenericM Maybe)
  where

    -- Variations on the show case company "genCom"
    genCom1 = everywhere (mkT (double "Joost")) genCom
    genCom2 = everywhere (mkT (double "Marlow")) genCom
    double x (E p@(P y _) (S s)) | x == y = E p (S (2*s))
    double _ e = e

    -- Sum up two salaries
    maxS (S x) (S y) = S (max x y)

    -- Make a two-arguments, generic function transformer
    mkTT :: (Typeable a, Typeable b, Typeable c)
         => (a -> a -> a) -> b -> c -> Maybe c
    mkTT (f::a -> a -> a) x y =
      case (cast x,cast y) of
        (Just (x'::a),Just (y'::a)) -> cast (f x' y')
        _                           -> Nothing

output = Just (C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8000.0))
           [PU (E (P "Joost" "Amsterdam") (S 2000.0))
           ,PU (E (P "Marlow" "Cambridge") (S 4000.0))]
           ,D "Strategy" (E (P "Blair" "London") (S 100000.0)) []])
