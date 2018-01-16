{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Prim
import GHC.Types

main :: IO ()
main = print (I# (1# +# 2# *# 3# +# 4#))

