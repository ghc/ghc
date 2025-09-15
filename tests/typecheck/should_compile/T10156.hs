{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module T10156 where

import Data.Coerce

data Iso a b = Iso (a -> b) (b -> a)

coerceIso :: Coercible a b => Iso a b
coerceIso = Iso coerce coerce

type family F x

f :: (Coercible a (F b), Coercible c (F b)) => a -> b -> c
f x _ = coerce x
