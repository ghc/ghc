{-# LANGUAGE Haskell2010 #-}
module ParserNoBinaryLiterals1 where

f :: Int -> ()
f 0b0 = ()
f _   = ()
