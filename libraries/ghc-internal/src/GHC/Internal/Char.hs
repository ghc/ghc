{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Internal.Char
    ( -- * Utilities
      chr

      -- * Monomorphic equality operators
      -- | See GHC.Internal.Classes#matching_overloaded_methods_in_rules
    , eqChar, neChar
    ) where

import GHC.Internal.Classes (eqChar, neChar)
import GHC.Internal.Base (otherwise, (++))
import GHC.Internal.Err (error)
import GHC.Internal.Show
import GHC.Internal.Prim (chr#, int2Word#, leWord#, Int#, Char#)
import GHC.Internal.Types (Char(..), Int(..), isTrue#)

-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
chr :: Int -> Char
chr (I# i#) = C# (safe_chr# i#)

{-# INLINABLE safe_chr# #-}
safe_chr# :: Int# -> Char#
safe_chr# i#
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = chr# i#
 | otherwise = chr_error i#

{-# NOINLINE chr_error #-}
chr_error :: Int# -> Char#
chr_error i# = error ("Data.Char.chr: argument outside Unicode range: 0..1114111: " ++ showSignedInt (I# 9#) (I# i#) "")
-- It's not really "Data.Char", but we assume that
-- the majority of users will import it from "base:Data.Char"
-- and not from "ghc-internal:GHC.Internal.Char".
