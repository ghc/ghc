{-# LANGUAGE DataKinds, PolyKinds, ExplicitForAll, MagicHash, UnboxedTuples,
             TypeFamilies, GADTs #-}

module Main where

import GHC.Exts

idint :: forall (a :: TYPE IntRep). a -> a
idint x = x

five _ = idint 3# +# idint 2#

type family F a where
  F Int = (# Bool, Int# #)
  F Char = (# Double, Int# #)

data G a where
  GInt :: G Int
  GChar :: G Char

f :: G a -> F a
f GInt = (# True, 3# #)
f GChar = (# 3.14, 5# #)

f' :: G a -> F a
f' GInt = (# False, 7# #)
f' GChar = (# 2.71829, 11# #)

g :: (# Bool, Int# #) -> String
g (# b, x #) = show b ++ " " ++ show (I# x)

h :: (# Double, Int# #) -> String
h (# d, x #) = show d ++ " " ++ show (I# x)

cond :: forall (a :: TYPE (TupleRep [LiftedRep, IntRep])). Bool -> a -> a -> a
cond True x _ = x
cond False _ x = x

main :: IO ()
main = do
  print (I# (five ()))
  putStrLn (g (f GInt))
  putStrLn (g (cond False (f GInt) (f' GInt)))
  putStrLn (h (cond True (f GChar) (f' GChar)))
