%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Bits]{The @Bits@ interface}

Defines the @Bits@ class containing bit-based operations.
See library document for details on the semantics of the
individual operations.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Bits where

import PrelBase

infixl 8 `shift`, `rotate`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

class Bits a where
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

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
shiftL  a i = shift  a i
shiftR  a i = shift  a (-i)
rotateL a i = rotate a i
rotateR a i = rotate a (-i)
\end{code}
