{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Internal.Char
    ( -- * Utilities
      chr

      -- * Monomorphic equality operators
      -- | See GHC.Internal.Classes#matching_overloaded_methods_in_rules
    , eqChar, neChar
    ) where

import GHC.Internal.Base
import GHC.Internal.Show

-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = errorWithoutStackTrace ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")

