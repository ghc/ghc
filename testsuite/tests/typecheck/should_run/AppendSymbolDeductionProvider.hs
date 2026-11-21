{-# LANGUAGE GHC2024, TypeFamilies #-}
module AppendSymbolDeductionProvider where

import GHC.TypeLits (AppendSymbol, Symbol)

type role Mark nominal
data Mark (s :: Symbol) = Mark Int

{-# INLINE removePrefix #-}
removePrefix :: (AppendSymbol "pre" s ~ "prefix") => Mark s -> Mark "fix"
removePrefix x = x

{-# INLINE removeSuffix #-}
removeSuffix :: (AppendSymbol s "fix" ~ "prefix") => Mark s -> Mark "pre"
removeSuffix x = x
