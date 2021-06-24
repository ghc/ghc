module OpaqueParseWarn1 where

f :: Num a => a -> a
f = (+1)
{-# OPAQUE f #-}
{-# SPECIALISE f :: Int -> Int #-}
