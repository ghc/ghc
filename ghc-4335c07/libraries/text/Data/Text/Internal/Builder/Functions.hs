{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Builder.Functions
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Useful functions and combinators.

module Data.Text.Internal.Builder.Functions
    (
      (<>)
    , i2d
    ) where

import Data.Monoid (mappend)
import Data.Text.Lazy.Builder (Builder)
import GHC.Base (chr#,ord#,(+#),Int(I#),Char(C#))
import Prelude ()

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))

-- | The normal 'mappend' function with right associativity instead of
-- left.
(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}

infixr 4 <>
