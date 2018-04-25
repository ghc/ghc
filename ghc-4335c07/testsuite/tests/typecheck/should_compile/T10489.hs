module T10489 where

-- Triggered an ASSERT in a debug build at some point.
convert d = let d' = case d of '0' -> '!' in d'
