{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Prim.Ext
import Control.Exception.Base
import GHC.Exts

main :: IO ()
main = do

   raiseAbsentSumField
      `catch` \AbsentSumFieldError -> putStrLn "Caught AbsentSumFieldError"
   
   let
      s :: (# Int# | Int #)
      !s = (# 10# | #)

   case unsafeCoerce# s of
      -- refer to GHC.Stg.Unarise for the layering down of unboxed sums into
      -- unboxed sums
      (# tag :: Int#, lrubbish :: Int, rubbish :: Int# #) -> putStrLn (show lrubbish)
