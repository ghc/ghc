
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}


module Test19 where

import PrelST
import PrelBase
import PrelErr

newtype IIO a = IIO (State# RealWorld -> (# State# RealWorld, a #))

unIIO :: IIO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIIO (IIO a) = a

instance  Functor IIO where
   fmap f x = x >>= (return . f)

instance  Monad IIO  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      =  m >>= \ _ -> k
    return x	= returnIIO x

    m >>= k     = bindIIO m k
    fail s	= error s -- not ioError?


bindIIO :: IIO a -> (a -> IIO b) -> IIO b
bindIIO (IIO m) k = IIO ( \ s ->
  case m s of 
    (# new_s, a #) -> unIIO (k a) new_s
  )

returnIIO :: a -> IIO a
returnIIO x = IIO (\ s -> (# s, x #))
