{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Exts

main :: IO ()
main = do

   let
      s :: (# Int# | Int #)
      !s = (# 10# | #)

   case unsafeCoerce# s of
      -- refer to GHC.Stg.Unarise for the layering down of unboxed sums into
      -- unboxed tuples
      (# tag :: Int#, lrubbish :: Int, rubbish :: Int# #) -> putStrLn (show lrubbish)
