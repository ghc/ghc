{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module T12150 where

data Result a = Success a | Error String

{- 80 guards

   ghc-7.10.3 -O :  0.3s
   ghc-8.0.1 -O  :  1.8s

   Increased to 450 guards in June 2020, along with increasing size of
   acceptance threshold. 0.4s compile time
-}

instance Functor Result where
    {-# INLINE fmap #-}
    fmap | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f
         | bool = f

      where
        bool = undefined
        f = undefined
