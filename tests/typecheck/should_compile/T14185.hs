{-# LANGUAGE MagicHash, UnboxedTuples, MultiParamTypeClasses, TypeFamilies,
  FunctionalDependencies, KindSignatures, PolyKinds, DataKinds,
  UndecidableInstances #-}
module T14185 where

import GHC.Types
import GHC.Exts


class Unbox (t :: *) (r :: TYPE k) | t -> r, r -> t where
  unbox :: t -> r
  box :: r -> t

instance Unbox Int Int# where
  unbox (I# i) = i
  box i = I# i

instance Unbox Char Char# where
  unbox (C# c) = c
  box c = C# c

instance (Unbox a a', Unbox b b') => Unbox (a,b) (# a', b' #) where
  unbox (a,b) = (# unbox a, unbox b #)
  box (# a, b #) = (box a, box b)

testInt :: Int
testInt = box (unbox 1)

testTup :: (Int, Char)
testTup = box (unbox (1, 'a'))
