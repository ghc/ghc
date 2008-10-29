{-# OPTIONS_GHC -fglasgow-exts -O -ddump-simpl -fno-method-sharing #-}
module Roman where

import Control.Monad.ST

newtype T s a = T { unT :: Int -> ST s a }

instance Monad (T s) where
   return = T . const . return
   T p >>= f = T $ \i -> do { x <- p i
  			    ; unT (f x) i }

myIndex :: T s Int
{-# INLINE myIndex #-}
myIndex = T return

foo :: T s Int
foo = do { x <- myIndex
         ; return (x + 1) }


{-   At one stage we got code looking like this:

    U.a3 =
       \ (@ s_a8E) (i_shA :: GHC.Base.Int) (eta_shB :: GHC.Prim.State# s_a8E) ->
         case ((((U.myIndex @ s_a8E)
                 `cast` ...)
                  i_shA)
               `cast` ...)
                eta_shB
         of wild_si5 { (# new_s_shF, r_shG #) -> ...

    U.foo :: forall s_a5S. U.T s_a5S GHC.Base.Int
    U.foo = U.a3 `cast` ...


The point is that myIndex should be inlined, else code is bad -}