module T12150 where

data Result a = Success a | Error String

{- 80 guards

   ghc-7.10.3 -O :  0.3s
   ghc-8.0.1 -O  :  1.8s
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

      where
        bool = undefined
        f = undefined
