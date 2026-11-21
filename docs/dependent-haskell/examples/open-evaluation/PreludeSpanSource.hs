{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeSpanSource (Span, SpanPrefix, SpanSuffix) where

import GHC.Exts (U)

-- | Use the installed pair-producing function in a type.
type Span :: (a %U -> Bool) -> [a] -> ([a], [a])
type Span p xs = span p xs

-- | Use the installed first projection on the checked pair.
type SpanPrefix :: (a %U -> Bool) -> [a] -> [a]
type SpanPrefix p xs = fst (Span p xs)

-- | Use the installed second projection on the checked pair.
type SpanSuffix :: (a %U -> Bool) -> [a] -> [a]
type SpanSuffix p xs = snd (Span p xs)
