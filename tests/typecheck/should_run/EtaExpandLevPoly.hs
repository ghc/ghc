{-# LANGUAGE UnboxedTuples, MagicHash, GADTs,
             DataKinds, PolyKinds, ExplicitForAll #-}

module Main where

import GHC.Exts

data G a where
  MkG :: G (TupleRep [BoxedRep Lifted, IntRep])

-- tests that we don't eta-expand functions that are representation-polymorphic
-- see CoreArity.mkEtaWW
foo :: forall a (b :: TYPE a). G a -> b -> b
foo MkG = (\x -> x) :: forall (c :: TYPE (TupleRep [BoxedRep Lifted, IntRep])). c -> c

data H a where
  MkH :: H IntRep

-- tests that we don't push coercions that make args representation-polymorphic
-- see Simplify.simplCast
bar :: forall (r :: RuntimeRep) (a :: TYPE r). H r -> (a -> a -> (# a, a #)) -> a -> (# a, a #)
bar MkH = (\f x -> f x x) :: forall (b :: TYPE IntRep). (b -> b -> (# b, b #)) -> b -> (# b, b #)

main :: IO ()
main = do
  let (# b, x #) = foo MkG (# True, 3# #)
  print b
  print (I# x)

  let (# y, z #) = bar MkH (#,#) 8#
  print (I# y)
  print (I# z)
