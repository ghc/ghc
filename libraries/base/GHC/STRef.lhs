\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module GHC.STRef where

import GHC.ST
import GHC.Base

data STRef s a = STRef (MutVar# s a)

newSTRef :: a -> ST s (STRef s a)
newSTRef init = ST $ \s1# ->
    case newMutVar# init s1#            of { (# s2#, var# #) ->
    (# s2#, STRef var# #) }

readSTRef :: STRef s a -> ST s a
readSTRef (STRef var#) = ST $ \s1# -> readMutVar# var# s1#

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var#) val = ST $ \s1# ->
    case writeMutVar# var# val s1#      of { s2# ->
    (# s2#, () #) }

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = readSTRef ref >>= writeSTRef ref . f

-- Just pointer equality on mutable references:
instance Eq (STRef s a) where
    STRef v1# == STRef v2# = sameMutVar# v1# v2#
\end{code}
