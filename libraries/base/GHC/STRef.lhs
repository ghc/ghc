\begin{code}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.STRef
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- References in the 'ST' monad.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.STRef (
        STRef(..),
        newSTRef, readSTRef, writeSTRef
    ) where

import GHC.ST
import GHC.Base

data STRef s a = STRef (MutVar# s a)
-- ^ a value of type @STRef s a@ is a mutable variable in state thread @s@,
-- containing a value of type @a@

-- |Build a new 'STRef' in the current state thread
newSTRef :: a -> ST s (STRef s a)
newSTRef init = ST $ \s1# ->
    case newMutVar# init s1#            of { (# s2#, var# #) ->
    (# s2#, STRef var# #) }

-- |Read the value of an 'STRef'
readSTRef :: STRef s a -> ST s a
readSTRef (STRef var#) = ST $ \s1# -> readMutVar# var# s1#

-- |Write a new value into an 'STRef'
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var#) val = ST $ \s1# ->
    case writeMutVar# var# val s1#      of { s2# ->
    (# s2#, () #) }

-- Just pointer equality on mutable references:
instance Eq (STRef s a) where
    STRef v1# == STRef v2# = isTrue# (sameMutVar# v1# v2#)

\end{code}
