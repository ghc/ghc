%
% (c) The GRASP/AQUA Project, Glasgow University, 1998-2000
%
\section[Bits]{The @Bits@ interface}

Defines the @Bits@ class containing bit-based operations.
See library document for details on the semantics of the
individual operations.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
#include "MachDeps.h"

module PrelBits where

#ifdef __GLASGOW_HASKELL__
import PrelGHC
import PrelBase
import PrelNum
#endif

--ADR: The fixity for .|. conflicts with that for .|. in Fran.
--     Removing all fixities is a fairly safe fix; fixing the "one fixity
--     per symbol per program" limitation in Hugs would take a lot longer.
#ifndef __HUGS__
infixl 8 `shift`, `rotate`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.
#endif

class Num a => Bits a where
    (.&.), (.|.), xor :: a -> a -> a
    complement        :: a -> a
    shift             :: a -> Int -> a
    rotate            :: a -> Int -> a
    bit               :: Int -> a
    setBit            :: a -> Int -> a
    clearBit          :: a -> Int -> a
    complementBit     :: a -> Int -> a
    testBit           :: a -> Int -> Bool
    bitSize           :: a -> Int
    isSigned          :: a -> Bool

    bit i               = 1 `shift` i
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i
    x `testBit` i       = (x .&. bit i) /= 0

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
x `shiftL`  i = x `shift`  i
x `shiftR`  i = x `shift`  (-i)
x `rotateL` i = x `rotate` i
x `rotateR` i = x `rotate` (-i)

instance Bits Int where
    (I# x#) .&.   (I# y#)  = I# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I# x#) .|.   (I# y#)  = I# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I# x#) `xor` (I# y#)  = I# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I# x#)     = I# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I# x#) `shift` (I# i#)
        | i# >=# 0#            = I# (x# `iShiftL#` i#)
        | otherwise            = I# (x# `iShiftRA#` negateInt# i#)
    (I# x#) `rotate` (I# i#) =
        I# (word2Int# ((x'# `shiftL#` i'#) `or#`
                       (x'# `shiftRL#` (wsib -# i'#))))
        where
        x'# = int2Word# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# (wsib -# 1#))
	wsib = WORD_SIZE_IN_BITS#   {- work around preprocessor problem (??) -}
    bitSize  _                 = WORD_SIZE_IN_BITS
    isSigned _                 = True
\end{code}
