{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module GHC.Integer.Internals (Integer(..)) where

import GHC.Prim (Int#, ByteArray#)

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

-- | Arbitrary-precision integers.
data Integer
   = S# Int#                            -- small integers
#ifndef ILX
   | J# Int# ByteArray#                 -- large integers
#else
   | J# Void BigInteger                 -- .NET big ints

foreign type dotnet "BigInteger" BigInteger
#endif

