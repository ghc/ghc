{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module T26465b where

-- Variant of T26465 which should be accepted

f :: Eq a => a -> Maybe a
f _ = Nothing

-- Monomorphism restriction bites
-- Eq a[tau:0] => a[tau:0] -> Maybe a[tau:0]
g = f

pattern P x <- ( g -> Just x )

x = g (1 :: Int)
