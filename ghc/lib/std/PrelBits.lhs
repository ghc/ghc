%
% (c) The GRASP/AQUA Project, Glasgow University, 1998-2000
%
\section[Bits]{The @Bits@ interface}

Defines the @Bits@ class containing bit-based operations.
See library document for details on the semantics of the
individual operations.

\begin{code}
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

  bit i             = shift 0x1 i
  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i       = (x .&. bit i) /= 0

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
shiftL  a i = shift  a i
shiftR  a i = shift  a (-i)
rotateL a i = rotate a i
rotateR a i = rotate a (-i)
\end{code}
