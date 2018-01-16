-- | Anti-Test for GHC 7.10+'s @BinaryLiterals@ extensions (see GHC #9224)
--
-- NB: This code won't compile with -XBinaryLiterals enabled

{-# LANGUAGE NegativeLiterals #-}

module Main where

main :: IO ()
main = print lst
  where
    -- "0b0" is to be parsed as "0 b0"
    lst = [ (,) 0b0, (,) 0b1, (,) 0b10, (,) 0b11
          , (,) -0b0, (,) -0b1, (,) -0b10, (,) -0b11
          ] :: [(Int,Int)]
    b0 = 60
    b1 = 61
    b11 = 611
    b10 = 610
