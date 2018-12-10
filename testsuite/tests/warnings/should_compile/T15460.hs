{-# LANGUAGE MagicHash #-}
module T15460 where

import GHC.Int

main :: IO ()
main = do
   let x = I# (0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff#)
   print x
