{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MonoPoly where

f :: Num a => a -> _
f x = x + y

y = f 1
