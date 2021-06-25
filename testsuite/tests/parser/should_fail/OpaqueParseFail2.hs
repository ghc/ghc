module OpaqueParseFail2 where

f = id
{-# OPAQUE SPECIALISE f :: Int -> Int #-}
