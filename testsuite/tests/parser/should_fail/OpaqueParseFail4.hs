module OpaqueParseWarn2 where

f :: Num a => a -> a
f = (+1)
{-# OPAQUE f #-}
{-# INLINE f #-}
