{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module CbvOverlap where

-- This is concerned with Note [Type equality cycles] and class lookup

class C a where
  meth :: a -> ()

instance C Int where
  meth _ = ()

type family F a

foo :: C (F a) => a -> Int -> ()
foo _ n = meth n
