{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
module T18888 where

f :: a %001 -> b
f x = undefined x
