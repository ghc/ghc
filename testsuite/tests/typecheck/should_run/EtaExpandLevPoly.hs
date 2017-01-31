{-# LANGUAGE UnboxedTuples, MagicHash, GADTs, TypeInType, ExplicitForAll #-}


module Main where

import GHC.Exts

data G a where
  MkG :: G (TupleRep [LiftedRep, IntRep])

-- tests that we don't eta-expand functions that are levity-polymorphic
-- see CoreArity.mkEtaWW
foo :: forall a (b :: TYPEvis a). G a -> b -> b
foo MkG = (\x -> x) :: forall (c :: TYPEvis (TupleRep [LiftedRep, IntRep])). c -> c

data H a where
  MkH :: H IntRep

-- tests that we don't push coercions that make args levity-polymorphic
-- see Simplify.simplCast
bar :: forall (r :: RuntimeRep) (a :: TYPEvis r). H r -> (a -> a -> (# a, a #)) -> a -> (# a, a #)
bar MkH = (\f x -> f x x) :: forall (b :: TYPEvis IntRep). (b -> b -> (# b, b #)) -> b -> (# b, b #)

main :: IO ()
main = do
  let (# b, x #) = foo MkG (# True, 3# #)
  print b
  print (I# x)

  let (# y, z #) = bar MkH (#,#) 8#
  print (I# y)
  print (I# z)
