{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fdefer-type-errors -O #-}
-- Oddly this bug was only triggered with -fdefer-type-errors
-- The -O ensures that the RULE is processed

module T14732 where

import Prelude hiding (zip, zipWith)

zipWith :: (a -> b -> c)
        -> Bundle v a
        -> Bundle v b
        -> Bundle v c
zipWith = undefined

class GVector (v :: * -> *) a
instance GVector Vector a

data Bundle (v :: * -> *) a
data Vector a
class Unbox a

stream :: GVector v a => v a -> Bundle v a
{-# INLINE [1] stream #-}
stream = undefined

zip :: (Unbox a, Unbox b) => Vector a -> Vector b -> Vector (a, b)
{-# INLINE [1] zip #-}
zip = undefined
{-# RULES "stream/zip [Vector.Unboxed]" forall as bs .
  stream (zip as bs) = zipWith (,) (stream as)
                                   (stream bs)   #-}
