{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module GHC.Integer.Type (Integer(..)) where

import GHC.Prim

default ()

-- | Arbitrary-precision integers.
data Integer
   = S# Int#                            -- small integers
   | J# Int# ByteArray#                 -- large integers

