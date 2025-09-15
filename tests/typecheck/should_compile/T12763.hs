{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -ddump-types #-}

module T12763 where

class C a | -> a where
   m :: a -> ()

instance C Int where
  m = undefined

-- Expecting inferred type f :: Int -> ()
f x = m x
