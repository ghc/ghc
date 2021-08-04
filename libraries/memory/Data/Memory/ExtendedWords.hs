-- |
-- Module      : Data.Memory.ExtendedWords
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Extra Word size
--
module Data.Memory.ExtendedWords
    ( Word128(..)
    ) where

import Data.Word (Word64)

-- | A simple Extended Word128 composed of 2 Word64
data Word128 = Word128 !Word64 !Word64 deriving (Show, Eq)
